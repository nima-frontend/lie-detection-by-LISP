;;; LieDetecion.lisp
;;; forward-chaining expert system in Common Lisp
;;; Production-ready version with comprehensive lie detection rules
;;;
;;; Copyright (c) 2025
;;; Licensed under MIT License

(in-package :cl-user)

(defpackage :expert
  (:use :cl)
  (:export 
   ;; Main API
   #:show-rules
   #:show-facts
   #:show-agenda
   #:assert-utterance
   #:assert-observation
   #:run
   #:clear-facts-and-agenda
   #:find-facts
   #:retract-fact-by-id
   ;; Globals (for advanced users)
   #:*facts*
   #:*rules*
   #:*rule-list*
   #:*agenda*))

(in-package :expert)

;;; -------------------------
;;; Data Structures & Globals
;;; -------------------------

(defstruct es-rule
  "Expert system rule structure"
  id          ; unique identifier
  name        ; human-readable name
  keywords    ; list of (word . weight) pairs for pattern matching
  salience    ; priority (higher fires first)
  action)     ; function to execute when rule fires

(defparameter *rules* (make-hash-table :test 'equal)
  "Hash table of all registered rules (keyed by ID)")

(defparameter *rule-list* '()
  "List of all rules (preserves insertion order)")

(defparameter *facts* '()
  "Working memory: list of facts (plists)")

(defparameter *next-fact-id* 1
  "Auto-incrementing fact ID counter")

(defparameter *agenda* '()
  "Conflict set: list of planned rule firings")

;;; -------------------------
;;; Utility Functions
;;; -------------------------

(defun now ()
  "Return current Unix timestamp"
  (get-universal-time))

(defun string-contains-p (substr str)
  "Case-insensitive substring test. Returns T if substr occurs in str."
  (and substr str (stringp substr) (stringp str)
       (search (string-downcase substr) (string-downcase str) :test #'char-equal)))

;;; -------------------------
;;; Fact Management
;;; -------------------------

(defun make-fact (type &rest kvs)
  "Create a fact plist with auto-generated ID and timestamp.
   Example: (make-fact :utterance :text \"He avoided eye contact\")"
  (let ((id *next-fact-id*))
    (incf *next-fact-id*)
    (list* :id id :type type :time (now) kvs)))

(defun assert-fact (&rest fact-plist)
  "Assert a fact into working memory. Returns the fact with generated :id."
  (let* ((fact (if (and fact-plist (eq (first fact-plist) :id))
                   fact-plist
                   (apply #'make-fact fact-plist))))
    (push fact *facts*)
    fact))

(defun retract-fact-by-id (id)
  "Remove a fact from working memory by ID"
  (setf *facts* (remove-if (lambda (f) (= (getf f :id) id)) *facts*))
  nil)

(defun find-facts (&key type)
  "Query facts by type. If no type given, returns all facts."
  (if type
      (remove-if-not (lambda (f) (eq (getf f :type) type)) *facts*)
      *facts*))

(defun show-facts ()
  "Display all facts in working memory"
  (format t "~%╔════════════════════════════════════════════════════════════╗~%")
  (format t "║ Facts in Working Memory: ~d~36T║~%" (length *facts*))
  (format t "╚════════════════════════════════════════════════════════════╝~%")
  (if *facts*
      (dolist (f *facts*)
        (format t "  • ID:~a Type:~a~%    ~a~%" 
                (getf f :id) 
                (getf f :type)
                (remove-from-plist f :id :type :time)))
      (format t "  (No facts asserted)~%"))
  nil)

(defun remove-from-plist (plist &rest keys)
  "Helper to remove keys from plist for display"
  (loop for (k v) on plist by #'cddr
        unless (member k keys)
        append (list k v)))

;;; -------------------------
;;; Rule Management
;;; -------------------------

(defun register-rule (rule)
  "Register a rule in the system"
  (setf (gethash (es-rule-id rule) *rules*) rule)
  (pushnew rule *rule-list* :key #'es-rule-id)
  rule)

(defmacro defrule (name &rest args)
  "Define a forward-chaining rule.
   
   Usage:
     (defrule rule-name 
       :id unique-id 
       :salience priority-number 
       :keywords ((\"keyword\" . weight) ...)
       body-forms...)
   
   Variables available in body: RULE, FACT, SCORE"
  (let* ((id (getf args :id (gensym "RID-")))
         (salience (getf args :salience 0))
         (keywords (getf args :keywords))
         (body (copy-list args)))
    ;; Extract body by removing all keyword arguments
    (dolist (key '(:id :salience :keywords))
      (let ((pos (position key body)))
        (when pos
          (setf body (append (subseq body 0 pos) (subseq body (+ pos 2)))))))
    `(let ((rule (make-es-rule 
                   :id ',id
                   :name ',name
                   :keywords (list ,@(mapcar (lambda (kw)
                                              `(cons ,(car kw) ,(cdr kw)))
                                            keywords))
                   :salience ,salience
                   :action (lambda (rule fact score)
                             (declare (ignorable rule fact score))
                             ,@body))))
       (register-rule rule)
       rule)))

(defun show-rules ()
  "Display all registered rules"
  (format t "~%╔════════════════════════════════════════════════════════════╗~%")
  (format t "║ Registered Rules: ~d~39T║~%" (length *rule-list*))
  (format t "╚════════════════════════════════════════════════════════════╝~%")
  (dolist (r *rule-list*)
    (format t "  • [~a] ~a (salience: ~a)~%    Keywords: ~a~%"
            (es-rule-id r) 
            (es-rule-name r) 
            (es-rule-salience r) 
            (mapcar #'car (es-rule-keywords r))))
  nil)

;;; -------------------------
;;; Pattern Matching & Scoring
;;; -------------------------

(defun score-rule-on-fact (rule fact)
  "Compute match score between RULE and FACT based on keyword presence.
   Returns a float score (0.0 = no match, higher = better match)"
  (let* ((text (or (getf fact :text) (getf fact :description) ""))
         (score 0.0))
    (dolist (kw (es-rule-keywords rule))
      (let ((word (car kw))
            (weight (cdr kw)))
        (when (string-contains-p word text)
          (incf score (or weight 1.0)))))
    score))

;;; -------------------------
;;; Agenda Management (Conflict Resolution)
;;; -------------------------

(defun clear-agenda () 
  "Clear the conflict set"
  (setf *agenda* '()))

(defun enqueue-firing (rule fact score)
  "Add a planned rule firing to the agenda"
  (push (list :rule rule :fact fact :score score :time (now)) *agenda*))

(defun build-agenda ()
  "Scan all rules against all facts and build the conflict set.
   Sorts by: 1) salience (desc), 2) score (desc), 3) recency (desc)"
  (clear-agenda)
  (dolist (rule *rule-list*)
    (dolist (fact *facts*)
      (when (eq (getf fact :type) :utterance)
        (let ((score (score-rule-on-fact rule fact)))
          (when (> score 0.0)
            (enqueue-firing rule fact score))))))
  ;; Conflict resolution strategy
  (setf *agenda*
        (sort *agenda*
              (lambda (a b)
                (let* ((ra (getf a :rule)) (rb (getf b :rule))
                       (sal-a (or (es-rule-salience ra) 0)) 
                       (sal-b (or (es-rule-salience rb) 0))
                       (score-a (getf a :score)) 
                       (score-b (getf b :score))
                       (time-a (getf a :time)) 
                       (time-b (getf b :time)))
                  (cond
                    ((/= sal-a sal-b) (> sal-a sal-b))      ; Salience first
                    ((/= score-a score-b) (> score-a score-b)) ; Then score
                    (t (> time-a time-b))))))))             ; Then recency

(defun show-agenda ()
  "Display the current conflict set"
  (format t "~%╔════════════════════════════════════════════════════════════╗~%")
  (format t "║ Agenda (Conflict Set): ~d items~30T║~%" (length *agenda*))
  (format t "╚════════════════════════════════════════════════════════════╝~%")
  (if *agenda*
      (dolist (item *agenda*)
        (format t "  • Rule: ~a (~a)~%    Fact: ~a | Score: ~,2f~%"
                (es-rule-id (getf item :rule))
                (es-rule-name (getf item :rule))
                (getf (getf item :fact) :id)
                (getf item :score)))
      (format t "  (No rules to fire)~%")))

;;; -------------------------
;;; Inference Engine
;;; -------------------------

(defun fire-one (agenda-item)
  "Execute the action for a single rule firing"
  (let* ((rule (getf agenda-item :rule))
         (fact (getf agenda-item :fact))
         (score (getf agenda-item :score)))
    (handler-case
        (funcall (es-rule-action rule) rule fact score)
      (error (e) 
        (format t "~%[ERROR] Rule ~a failed: ~a~%" (es-rule-name rule) e)))))

(defun run (&optional (limit 1000))
  "Execute the forward-chaining inference engine.
   
   Builds the agenda and fires rules in priority order.
   Limit parameter prevents infinite loops (default: 1000 firings)"
  (let ((fires 0)
        (initial-fact-count (length *facts*)))
    (format t "~%╔════════════════════════════════════════════════════════════╗~%")
    (format t "║ Running Inference Engine...~33T║~%")
    (format t "╚════════════════════════════════════════════════════════════╝~%")
    (build-agenda)
    (show-agenda)
    (format t "~%Firing rules...~%")
    (loop while (and *agenda* (< fires limit))
          do (let ((item (pop *agenda*)))
               (fire-one item)
               (incf fires)))
    (format t "~%╔════════════════════════════════════════════════════════════╗~%")
    (format t "║ Inference Complete~42T║~%")
    (format t "╠════════════════════════════════════════════════════════════╣~%")
    (format t "║ Rules fired: ~d~45T║~%" fires)
    (format t "║ Facts: ~d → ~d~45T║~%" initial-fact-count (length *facts*))
    (format t "╚════════════════════════════════════════════════════════════╝~%")
    fires))

;;; -------------------------
;;; System Utilities
;;; -------------------------

(defun clear-facts-and-agenda ()
  "Reset the system to initial state"
  (setf *facts* '())
  (setf *agenda* '())
  (setf *next-fact-id* 1)
  (format t "~%✓ System reset: facts and agenda cleared~%"))

(defmacro assert-utterance (text &rest kvs)
  "Convenience macro: assert an :utterance fact with :text"
  `(assert-fact :utterance :text ,text ,@kvs))

(defmacro assert-observation (text &rest kvs)
  "Alias for assert-utterance"
  `(assert-utterance ,text ,@kvs))

;;; =========================================================================
;;; LIE DETECTION RULES
;;; =========================================================================
;;; Based on research in behavioral psychology and deception detection.
;;; Rules use keyword pattern matching with weighted scoring.
;;; Higher salience = higher priority in conflict resolution.
;;; =========================================================================

;; HIGH PRIORITY RULES (Salience 4-5): Strong indicators

(defrule mismatched-words-and-body-language
  :id 1 :salience 5
  :keywords (("mismatch" . 1.0) ("inconsistent" . 0.8) ("contradict" . 0.8))
  (format t "  ✗ [DETECTION] Mismatched words/body language (Fact ~a, Score: ~,2f)~%" 
          (getf fact :id) score)
  (assert-fact :detection :rule-id 1 :name 'mismatched-words-and-body-language 
               :confidence :high :score score :fact-id (getf fact :id)))

(defrule excessive-eye-contact
  :id 2 :salience 4
  :keywords (("stare" . 1.0) ("staring" . 1.0) ("excessive eye contact" . 0.9) 
             ("intense gaze" . 0.8) ("unblinking" . 0.7))
  (format t "  ✗ [DETECTION] Excessive eye contact (Fact ~a, Score: ~,2f)~%" 
          (getf fact :id) score)
  (assert-fact :detection :rule-id 2 :name 'excessive-eye-contact 
               :confidence :high :score score :fact-id (getf fact :id)))

(defrule avoiding-eye-contact
  :id 3 :salience 4
  :keywords (("avoid eye" . 1.0) ("look away" . 0.9) ("looking away" . 0.9)
             ("avert" . 0.8) ("glance away" . 0.7))
  (format t "  ✗ [DETECTION] Avoiding eye contact (Fact ~a, Score: ~,2f)~%" 
          (getf fact :id) score)
  (assert-fact :detection :rule-id 3 :name 'avoiding-eye-contact 
               :confidence :high :score score :fact-id (getf fact :id)))

(defrule rapid-blinking
  :id 4 :salience 4
  :keywords (("rapid blink" . 1.0) ("blinking rapidly" . 1.0) ("blink fast" . 0.9)
             ("eye flutter" . 0.8))
  (format t "  ✗ [DETECTION] Rapid blinking (Fact ~a, Score: ~,2f)~%" 
          (getf fact :id) score)
  (assert-fact :detection :rule-id 4 :name 'rapid-blinking 
               :confidence :high :score score :fact-id (getf fact :id)))

;; MEDIUM PRIORITY RULES (Salience 2-3): Moderate indicators

(defrule touching-mouth
  :id 5 :salience 3
  :keywords (("cover mouth" . 1.0) ("hand over mouth" . 1.0) ("touch lips" . 0.8) 
             ("touch mouth" . 0.8) ("hand to mouth" . 0.7))
  (format t "  ⚠ [DETECTION] Touching/covering mouth (Fact ~a, Score: ~,2f)~%" 
          (getf fact :id) score)
  (assert-fact :detection :rule-id 5 :name 'touching-mouth 
               :confidence :medium :score score :fact-id (getf fact :id)))

(defrule pulling-ear
  :id 6 :salience 3
  :keywords (("pull ear" . 1.0) ("tug earlobe" . 1.0) ("touch ear" . 0.8) 
             ("scratch ear" . 0.7))
  (format t "  ⚠ [DETECTION] Pulling/touching ear (Fact ~a, Score: ~,2f)~%" 
          (getf fact :id) score)
  (assert-fact :detection :rule-id 6 :name 'pulling-ear 
               :confidence :medium :score score :fact-id (getf fact :id)))

(defrule stroking-neck
  :id 7 :salience 3
  :keywords (("rub neck" . 1.0) ("stroke neck" . 1.0) ("touch neck" . 0.8) 
             ("scratch neck" . 0.7))
  (format t "  ⚠ [DETECTION] Stroking/rubbing neck (Fact ~a, Score: ~,2f)~%" 
          (getf fact :id) score)
  (assert-fact :detection :rule-id 7 :name 'stroking-neck 
               :confidence :medium :score score :fact-id (getf fact :id)))

(defrule jaw-clench
  :id 8 :salience 3
  :keywords (("clench jaw" . 1.0) ("jaw tight" . 0.9) ("tense jaw" . 0.8) 
             ("grind teeth" . 0.7))
  (format t "  ⚠ [DETECTION] Jaw clenching (Fact ~a, Score: ~,2f)~%" 
          (getf fact :id) score)
  (assert-fact :detection :rule-id 8 :name 'jaw-clench 
               :confidence :medium :score score :fact-id (getf fact :id)))

(defrule fidgeting-hands
  :id 9 :salience 3
  :keywords (("fidget" . 1.0) ("restless hands" . 0.9) ("hand movement" . 0.7) 
             ("play with hands" . 0.8))
  (format t "  ⚠ [DETECTION] Fidgeting hands (Fact ~a, Score: ~,2f)~%" 
          (getf fact :id) score)
  (assert-fact :detection :rule-id 9 :name 'fidgeting-hands 
               :confidence :medium :score score :fact-id (getf fact :id)))

(defrule feet-pointing-exit
  :id 10 :salience 2
  :keywords (("feet point" . 1.0) ("feet toward door" . 0.9) ("feet exit" . 0.8))
  (format t "  ⚠ [DETECTION] Feet pointing to exit (Fact ~a, Score: ~,2f)~%" 
          (getf fact :id) score)
  (assert-fact :detection :rule-id 10 :name 'feet-pointing-exit 
               :confidence :medium :score score :fact-id (getf fact :id)))

(defrule crossed-arms
  :id 11 :salience 2
  :keywords (("cross arms" . 1.0) ("arms crossed" . 1.0) ("fold arms" . 0.9) 
             ("defensive posture" . 0.8))
  (format t "  ⚠ [DETECTION] Crossed arms (defensive) (Fact ~a, Score: ~,2f)~%" 
          (getf fact :id) score)
  (assert-fact :detection :rule-id 11 :name 'crossed-arms 
               :confidence :medium :score score :fact-id (getf fact :id)))

(defrule shrinking-posture
  :id 12 :salience 2
  :keywords (("shrink" . 1.0) ("hunch" . 0.9) ("slouch" . 0.8) 
             ("make self small" . 0.8))
  (format t "  ⚠ [DETECTION] Shrinking posture (Fact ~a, Score: ~,2f)~%" 
          (getf fact :id) score)
  (assert-fact :detection :rule-id 12 :name 'shrinking-posture 
               :confidence :medium :score score :fact-id (getf fact :id)))

(defrule elevated-pitch
  :id 16 :salience 3
  :keywords (("voice higher" . 1.0) ("pitch up" . 0.9) ("high pitched" . 0.9) 
             ("squeaky" . 0.7))
  (format t "  ⚠ [DETECTION] Elevated voice pitch (Fact ~a, Score: ~,2f)~%" 
          (getf fact :id) score)
  (assert-fact :detection :rule-id 16 :name 'elevated-pitch 
               :confidence :medium :score score :fact-id (getf fact :id)))

(defrule sudden-volume-change
  :id 17 :salience 3
  :keywords (("volume change" . 1.0) ("suddenly loud" . 0.9) ("suddenly quiet" . 0.9) 
             ("voice drop" . 0.7))
  (format t "  ⚠ [DETECTION] Sudden volume change (Fact ~a, Score: ~,2f)~%" 
          (getf fact :id) score)
  (assert-fact :detection :rule-id 17 :name 'sudden-volume-change 
               :confidence :medium :score score :fact-id (getf fact :id)))

(defrule filler-usage
  :id 18 :salience 2
  :keywords (("um" . 1.0) ("uh" . 1.0) ("like" . 0.8) ("you know" . 0.7) 
             ("erm" . 0.9))
  (format t "  ⚠ [DETECTION] Excessive fillers (Fact ~a, Score: ~,2f)~%" 
          (getf fact :id) score)
  (assert-fact :detection :rule-id 18 :name 'filler-usage 
               :confidence :medium :score score :fact-id (getf fact :id)))

(defrule repeat-question
  :id 19 :salience 2
  :keywords (("repeat question" . 1.0) ("ask again" . 0.9) ("what did you say" . 0.8) 
             ("buying time" . 0.7))
  (format t "  ⚠ [DETECTION] Repeating question (stalling) (Fact ~a, Score: ~,2f)~%" 
          (getf fact :id) score)
  (assert-fact :detection :rule-id 19 :name 'repeat-question 
               :confidence :medium :score score :fact-id (getf fact :id)))

;; LOW PRIORITY RULES (Salience 1): Weaker indicators

(defrule overly-specific
  :id 20 :salience 1
  :keywords (("too specific" . 1.0) ("memorized" . 0.9) ("rehearsed" . 0.9) 
             ("scripted" . 0.8))
  (format t "  ℹ [DETECTION] Overly specific details (Fact ~a, Score: ~,2f)~%" 
          (getf fact :id) score)
  (assert-fact :detection :rule-id 20 :name 'overly-specific 
               :confidence :low :score score :fact-id (getf fact :id)))

(defrule change-details
  :id 21 :salience 1
  :keywords (("change story" . 1.0) ("different detail" . 0.9) ("contradiction" . 0.8))
  (format t "  ℹ [DETECTION] Changing details (Fact ~a, Score: ~,2f)~%" 
          (getf fact :id) score)
  (assert-fact :detection :rule-id 21 :name 'change-details 
               :confidence :low :score score :fact-id (getf fact :id)))

(defrule distancing-language
  :id 22 :salience 1
  :keywords (("that person" . 1.0) ("they" . 0.7) ("someone" . 0.6) 
             ("not using I" . 0.8))
  (format t "  ℹ [DETECTION] Distancing language (Fact ~a, Score: ~,2f)~%" 
          (getf fact :id) score)
  (assert-fact :detection :rule-id 22 :name 'distancing-language 
               :confidence :low :score score :fact-id (getf fact :id)))

(defrule covering-face
  :id 23 :salience 1
  :keywords (("cover face" . 1.0) ("hide face" . 0.9) ("hand on face" . 0.7))
  (format t "  ℹ [DETECTION] Covering face (Fact ~a, Score: ~,2f)~%" 
          (getf fact :id) score)
  (assert-fact :detection :rule-id 23 :name 'covering-face 
               :confidence :low :score score :fact-id (getf fact :id)))

(defrule dry-lips
  :id 24 :salience 1
  :keywords (("lick lips" . 1.0) ("bite lip" . 0.9) ("dry mouth" . 0.8))
  (format t "  ℹ [DETECTION] Dry lips/lip biting (Fact ~a, Score: ~,2f)~%" 
          (getf fact :id) score)
  (assert-fact :detection :rule-id 24 :name 'dry-lips 
               :confidence :low :score score :fact-id (getf fact :id)))

(defrule flush-face
  :id 25 :salience 1
  :keywords (("face red" . 1.0) ("blush" . 0.9) ("flushed" . 0.9) ("face color" . 0.7))
  (format t "  ℹ [DETECTION] Flushed face (Fact ~a, Score: ~,2f)~%" 
          (getf fact :id) score)
  (assert-fact :detection :rule-id 25 :name 'flush-face 
               :confidence :low :score score :fact-id (getf fact :id)))

(defrule body-turn-away
  :id 26 :salience 1
  :keywords (("turn away" . 1.0) ("body angle" . 0.8) ("torso away" . 0.8))
  (format t "  ℹ [DETECTION] Body turns away (Fact ~a, Score: ~,2f)~%" 
          (getf fact :id) score)
  (assert-fact :detection :rule-id 26 :name 'body-turn-away 
               :confidence :low :score score :fact-id (getf fact :id)))

(defrule nod-yes-say-no
  :id 27 :salience 2
  :keywords (("head shake" . 1.0) ("nod no" . 0.9) ("head contradict" . 0.9))
  (format t "  ⚠ [DETECTION] Head movement contradicts words (Fact ~a, Score: ~,2f)~%" 
          (getf fact :id) score)
  (assert-fact :detection :rule-id 27 :name 'nod-yes-say-no 
               :confidence :medium :score score :fact-id (getf fact :id)))

(defrule rapid-seat-shift
  :id 28 :salience 1
  :keywords (("shift seat" . 1.0) ("squirm" . 0.9) ("adjust position" . 0.7))
  (format t "  ℹ [DETECTION] Rapid seat shifting (Fact ~a, Score: ~,2f)~%" 
          (getf fact :id) score)
  (assert-fact :detection :rule-id 28 :name 'rapid-seat-shift 
               :confidence :low :score score :fact-id (getf fact :id)))

(defrule self-soothing
  :id 30 :salience 1
  :keywords (("self-sooth" . 1.0) ("hug self" . 0.9) ("comfort gesture" . 0.8))
  (format t "  ℹ [DETECTION] Self-soothing behavior (Fact ~a, Score: ~,2f)~%" 
          (getf fact :id) score)
  (assert-fact :detection :rule-id 30 :name 'self-soothing 
               :confidence :low :score score :fact-id (getf fact :id)))

(defrule foot-tapping
  :id 31 :salience 1
  :keywords (("tap foot" . 1.0) ("bounce leg" . 0.9) ("foot movement" . 0.7))
  (format t "  ℹ [DETECTION] Foot tapping (Fact ~a, Score: ~,2f)~%" 
          (getf fact :id) score)
  (assert-fact :detection :rule-id 31 :name 'foot-tapping 
               :confidence :low :score score :fact-id (getf fact :id)))

(defrule inappropriate-smile
  :id 32 :salience 1
  :keywords (("fake smile" . 1.0) ("forced smile" . 0.9) ("smile wrong time" . 0.8))
  (format t "  ℹ [DETECTION] Inappropriate smiling (Fact ~a, Score: ~,2f)~%" 
          (getf fact :id) score)
  (assert-fact :detection :rule-id 32 :name 'inappropriate-smile 
               :confidence :low :score score :fact-id (getf fact :id)))

(defrule throat-clearing
  :id 36 :salience 1
  :keywords (("clear throat" . 1.0) ("gulp" . 0.9) ("swallow hard" . 0.8) 
             ("throat" . 0.6))
  (format t "  ℹ [DETECTION] Throat clearing (Fact ~a, Score: ~,2f)~%" 
          (getf fact :id) score)
  (assert-fact :detection :rule-id 36 :name 'throat-clearing 
               :confidence :low :score score :fact-id (getf fact :id)))

(defrule hands-in-pockets
  :id 37 :salience 1
  :keywords (("hands in pockets" . 1.0) ("hide hands" . 0.9) ("pocket hands" . 0.8))
  (format t "  ℹ [DETECTION] Hands in pockets/hidden (Fact ~a, Score: ~,2f)~%" 
          (getf fact :id) score)
  (assert-fact :detection :rule-id 37 :name 'hands-in-pockets 
               :confidence :low :score score :fact-id (getf fact :id)))

(defrule quick-breathing
  :id 39 :salience 1
  :keywords (("breathe fast" . 1.0) ("rapid breath" . 0.9) ("shallow breath" . 0.8))
  (format t "  ℹ [DETECTION] Quick/shallow breathing (Fact ~a, Score: ~,2f)~%" 
          (getf fact :id) score)
  (assert-fact :detection :rule-id 39 :name 'quick-breathing 
               :confidence :low :score score :fact-id (getf fact :id)))

(defrule defensive-posture
  :id 40 :salience 1
  :keywords (("defensive" . 1.0) ("guard up" . 0.9) ("protective stance" . 0.8))
  (format t "  ℹ [DETECTION] Defensive posture (Fact ~a, Score: ~,2f)~%" 
          (getf fact :id) score)
  (assert-fact :detection :rule-id 40 :name 'defensive-posture 
               :confidence :low :score score :fact-id (getf fact :id)))

(defrule pause-and-rush
  :id 43 :salience 1
  :keywords (("long pause" . 1.0) ("then rush" . 0.9) ("pause rush" . 0.8) 
             ("hesitate rush" . 0.7))
  (format t "  ℹ [DETECTION] Pause then rush words (Fact ~a, Score: ~,2f)~%" 
          (getf fact :id) score)
  (assert-fact :detection :rule-id 43 :name 'pause-and-rush 
               :confidence :low :score score :fact-id (getf fact :id)))

(defrule over-grooming
  :id 44 :salience 1
  :keywords (("adjust clothes" . 1.0) ("fix hair" . 0.9) ("groom" . 0.8) 
             ("straighten" . 0.6))
  (format t "  ℹ [DETECTION] Over-grooming (Fact ~a, Score: ~,2f)~%" 
          (getf fact :id) score)
  (assert-fact :detection :rule-id 44 :name 'over-grooming 
               :confidence :low :score score :fact-id (getf fact :id)))

(defrule rigid-body
  :id 45 :salience 1
  :keywords (("rigid" . 1.0) ("frozen" . 0.9) ("stiff body" . 0.9) ("tense" . 0.7))
  (format t "  ℹ [DETECTION] Rigid/frozen body (Fact ~a, Score: ~,2f)~%" 
          (getf fact :id) score)
  (assert-fact :detection :rule-id 45 :name 'rigid-body 
               :confidence :low :score score :fact-id (getf fact :id)))

(defrule delay-start-speak
  :id 46 :salience 1
  :keywords (("delay answer" . 1.0) ("hesitate" . 0.9) ("pause before" . 0.8) 
             ("slow start" . 0.7))
  (format t "  ℹ [DETECTION] Delayed start speaking (Fact ~a, Score: ~,2f)~%" 
          (getf fact :id) score)
  (assert-fact :detection :rule-id 46 :name 'delay-start-speak 
               :confidence :low :score score :fact-id (getf fact :id)))

(defrule qualifiers-excessive
  :id 47 :salience 1
  :keywords (("honestly" . 1.0) ("to be honest" . 1.0) ("believe me" . 0.9) 
             ("I swear" . 0.9) ("truthfully" . 0.8))
  (format t "  ℹ [DETECTION] Excessive qualifiers (Fact ~a, Score: ~,2f)~%" 
          (getf fact :id) score)
  (assert-fact :detection :rule-id 47 :name 'qualifiers-excessive 
               :confidence :low :score score :fact-id (getf fact :id)))

(defrule hands-clasped
  :id 49 :salience 1
  :keywords (("clasp hands" . 1.0) ("wring hands" . 0.9) ("hands together" . 0.8))
  (format t "  ℹ [DETECTION] Hands clasped/wringing (Fact ~a, Score: ~,2f)~%" 
          (getf fact :id) score)
  (assert-fact :detection :rule-id 49 :name 'hands-clasped 
               :confidence :low :score score :fact-id (getf fact :id)))

(defrule shadowing-question
  :id 50 :salience 2
  :keywords (("echo question" . 1.0) ("repeat back" . 0.9) ("mirror question" . 0.8))
  (format t "  ⚠ [DETECTION] Shadowing/echoing question (Fact ~a, Score: ~,2f)~%" 
          (getf fact :id) score)
  (assert-fact :detection :rule-id 50 :name 'shadowing-question 
               :confidence :medium :score score :fact-id (getf fact :id)))

;;; -------------------------
;;; Startup Message
;;; -------------------------

(format t "~%╔════════════════════════════════════════════════════════════╗~%")
(format t "║                                                            ║~%")
(format t "║        LIE DETECTION EXPERT SYSTEM v1.0                   ║~%")
(format t "║        Common Lisp Forward-Chaining System                ║~%")
(format t "║                                                            ║~%")
(format t "╠════════════════════════════════════════════════════════════╣~%")
(format t "║ Available Commands:                                        ║~%")
(format t "║                                                            ║~%")
(format t "║  (expert:show-rules)              List all rules          ║~%")
(format t "║  (expert:assert-utterance TEXT)   Add observation         ║~%")
(format t "║  (expert:show-facts)              List all facts          ║~%")
(format t "║  (expert:run)                     Execute inference       ║~%")
(format t "║  (expert:clear-facts-and-agenda)  Reset system            ║~%")
(format t "║                                                            ║~%")
(format t "╠════════════════════════════════════════════════════════════╣~%")
(format t "║ Quick Start Example:                                       ║~%")
(format t "║                                                            ║~%")
(format t "║  (expert:assert-utterance                                 ║~%")
(format t "║    \"They avoided eye contact and blinked rapidly\")        ║~%")
(format t "║  (expert:run)                                             ║~%")
(format t "║                                                            ║~%")
(format t "╚════════════════════════════════════════════════════════════╝~%~%")

;;; End of LieDetection.lisp
