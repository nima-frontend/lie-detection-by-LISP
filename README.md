# Lie Detection Expert System

A forward-chaining expert system written in Common Lisp for detecting potential deception indicators based on behavioral analysis. this system uses pattern matching and rule-based inference to analyze textual observations of human behavior.

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Common Lisp](https://img.shields.io/badge/Common%20Lisp-SBCL%20%7C%20CCL-blue)](http://www.sbcl.org/)

## ⚠️ Disclaimer

This system is for **educational and research purposes only**. Lie detection through behavioral analysis is a complex field with significant controversy regarding accuracy and reliability. This system should NOT be used as the sole basis for making important decisions about truthfulness. Always consider:

- Context and individual differences in behavior
- Cultural variations in communication styles
- Stress and anxiety can produce similar behaviors
- Professional assessment requires training and multiple data points

## 🌟 Features

- **50+ Detection Rules**: Comprehensive rule base covering verbal and non-verbal deception indicators
- **Weighted Pattern Matching**: Keywords scored with confidence weights
- **Conflict Resolution**: Priority-based agenda with salience (importance) levels
- **Modular Architecture**: Easy to extend with new rules or modify existing ones
- **Interactive REPL**: Full integration with Common Lisp development environment
- **Production-Ready**: Error handling, proper exports, and clean API

## 📋 Requirements

- Common Lisp implementation (SBCL, CCL, or compatible)
- SLIME (optional, for Emacs integration)
- Quicklisp (optional, for package management)

## 🚀 Quick Start

### Installation

1. Clone the repository:
```bash
git clone https://github.com/nima-frontend/lie-detection-by-LISP.git
cd lie-detection-by-LISP
```

2. Load in your Lisp REPL:
```lisp
(load "LieDetection.lisp")
```

### Basic Usage

```lisp
;; 1. View available rules
(expert:show-rules)

;; 2. Assert an observation
(expert:assert-utterance 
  "They avoided eye contact and blinked rapidly while speaking")

;; 3. Run the inference engine
(expert:run)

;; 4. View detected indicators
(expert:show-facts)

;; 5. Reset for next analysis
(expert:clear-facts-and-agenda)
```

### Example Session

```lisp
CL-USER> (expert:assert-utterance 
           "The subject avoided eye contact, touched their mouth, and said 'honestly' multiple times")

CL-USER> (expert:run)
╔════════════════════════════════════════════════════════════╗
║ Running Inference Engine...                               ║
╚════════════════════════════════════════════════════════════╝

Agenda (Conflict Set): 3 items
  • Rule: 3 (AVOIDING-EYE-CONTACT)
    Fact: 1 | Score: 1.00
  • Rule: 5 (TOUCHING-MOUTH)
    Fact: 1 | Score: 0.80
  • Rule: 47 (QUALIFIERS-EXCESSIVE)
    Fact: 1 | Score: 1.00

Firing rules...
  ✗ [DETECTION] Avoiding eye contact (Fact 1, Score: 1.00)
  ⚠ [DETECTION] Touching/covering mouth (Fact 1, Score: 0.80)
  ℹ [DETECTION] Excessive qualifiers (Fact 1, Score: 1.00)

╔════════════════════════════════════════════════════════════╗
║ Inference Complete                                         ║
╠════════════════════════════════════════════════════════════╣
║ Rules fired: 3                                             ║
║ Facts: 1 → 4                                               ║
╚════════════════════════════════════════════════════════════╝
```

## 📚 Architecture

### Core Components

1. **Rule Engine**: Forward-chaining inference with conflict resolution
2. **Pattern Matcher**: Keyword-based text analysis with weighted scoring
3. **Working Memory**: Dynamic fact base with temporal information
4. **Agenda**: Priority queue of rule activations

### Rule Structure

```lisp
(defrule rule-name
  :id unique-id
  :salience priority        ; 1 (low) to 5 (high)
  :keywords (("pattern" . weight) ...)
  ;; Action body - fires when pattern matches
  (format t "Detection logic here")
  (assert-fact :detection ...))
```

### Confidence Levels

- **High (✗)**: Salience 4-5 - Strong indicators (e.g., eye contact patterns)
- **Medium (⚠)**: Salience 2-3 - Moderate indicators (e.g., vocal changes)
- **Low (ℹ)**: Salience 1 - Weak indicators (e.g., grooming behaviors)

## 🎯 Detection Categories

The system includes rules for:

- **Eye Behavior**: Contact avoidance, excessive staring, rapid blinking
- **Facial Cues**: Mouth touching, jaw clenching, facial flushing
- **Body Language**: Posture changes, fidgeting, defensive positions
- **Vocal Patterns**: Pitch changes, volume shifts, filler words
- **Speech Content**: Inconsistencies, distancing language, excessive qualifiers
- **Nervous Behaviors**: Self-soothing, grooming, throat clearing

## 🔧 API Reference

### Main Functions

#### `(show-rules)`
Display all registered rules with their priorities and keywords.

#### `(assert-utterance text &rest kvs)`
Add an observation to working memory.
```lisp
(assert-utterance "They looked away nervously" :source "interviewer")
```

#### `(run &optional limit)`
Execute the inference engine. Limit parameter prevents infinite loops (default: 1000).

#### `(show-facts)`
Display all facts in working memory, including detections.

#### `(clear-facts-and-agenda)`
Reset the system to initial state.

#### `(find-facts &key type)`
Query facts by type (`:utterance`, `:detection`, etc.)

### Advanced Usage

```lisp
;; Find all detection facts
(remove-if-not 
  (lambda (f) (eq (getf f :type) :detection)) 
  expert:*facts*)

;; Access raw data structures
expert:*rules*      ; Hash table of rules
expert:*rule-list*  ; Ordered list of rules
expert:*facts*      ; Working memory
expert:*agenda*     ; Current conflict set
```

## 🛠️ Extending the System

### Adding New Rules

```lisp
(defrule my-new-rule
  :id 51
  :salience 3
  :keywords (("nervous laugh" . 1.0) ("giggle" . 0.8))
  (format t "  ⚠ [DETECTION] Nervous laughter~%")
  (assert-fact :detection 
               :rule-id 51 
               :name 'my-new-rule
               :confidence :medium
               :score score
               :fact-id (getf fact :id)))
```

### Custom Fact Types

```lisp
;; Assert different fact types
(expert:assert-fact :video-analysis 
                    :subject "person-1"
                    :timestamp 12345
                    :description "Subject exhibits micro-expression")
```

## Contributing

Contributions welcome! Please:

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

### Development Guidelines

- Follow existing code style and documentation patterns
- Update README for significant changes
- Ensure rules have proper salience and keyword weights

## 📖 Background & Research

This system is based on research in:

- Behavioral psychology and deception detection
- Non-verbal communication analysis
- Forensic psychology
- Cognitive load theory

### Key References

- Ekman, P. (2009). *Telling Lies: Clues to Deceit in the Marketplace, Politics, and Marriage*
- Vrij, A. (2008). *Detecting Lies and Deceit: Pitfalls and Opportunities*
- DePaulo, B. M., et al. (2003). "Cues to deception." *Psychological Bulletin*

## 📝 License

MIT License - see [LICENSE](LICENSE) file for details
---

**Remember**: This is a tool for analysis and learning, not a definitive lie detector. Always use human judgment and multiple sources of information when assessing truthfulness.
