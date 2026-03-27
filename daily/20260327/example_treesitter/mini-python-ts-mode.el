;;; mini-python-ts-mode.el --- Minimal Python major mode using tree-sitter -*- lexical-binding: t -*-

;; Author: podhmo
;; Keywords: python languages tree-sitter
;; Version: 0.1.0

;;; Commentary:

;; `mini-python-ts-mode' is a minimal Python major mode that uses
;; Emacs's built-in tree-sitter support (Emacs 29+) for font-lock
;; (syntax highlighting).
;;
;; Unlike `python-ts-mode', this mode:
;;   - Does NOT inherit from `python-mode' or `python-base-mode'
;;   - Only configures font-lock; no indentation rules, no imenu (yet)
;;   - Is self-contained; does not depend on python.el internals
;;
;; Requirements:
;;   - Emacs 29 or later (built-in tree-sitter support)
;;   - tree-sitter Python grammar installed:
;;       M-x treesit-install-language-grammar RET python RET
;;
;; Usage:
;;   (load "/path/to/mini-python-ts-mode.el")
;;   ;; Then in a Python buffer:
;;   M-x mini-python-ts-mode
;;
;; Or, to use it automatically for .py files:
;;   (add-to-list 'auto-mode-alist '("\\.py\\'" . mini-python-ts-mode))

;;; Code:

(require 'treesit)

;;; Font-lock data variables

(defvar mini-python--treesit-keywords
  '("as" "assert" "async" "await" "break" "case" "class" "continue" "def"
    "del" "elif" "else" "except" "exec" "finally" "for" "from"
    "global" "if" "import" "lambda" "match" "nonlocal" "pass" "print"
    "raise" "return" "try" "while" "with" "yield"
    ;; Technically operators, but conventionally highlighted as keywords.
    "and" "in" "is" "not" "or" "not in" "is not")
  "Python keywords for tree-sitter font-lock.")

(defvar mini-python--treesit-builtin-types
  '("int" "float" "complex" "bool" "list" "tuple" "range" "str"
    "bytes" "bytearray" "memoryview" "set" "frozenset" "dict")
  "Python built-in type names for tree-sitter font-lock.")

(defvar mini-python--treesit-builtins
  (append mini-python--treesit-builtin-types
          '("abs" "all" "any" "ascii" "bin" "breakpoint"
            "callable" "chr" "classmethod" "compile"
            "delattr" "dir" "divmod" "enumerate" "eval" "exec"
            "filter" "format" "getattr" "globals"
            "hasattr" "hash" "help" "hex" "id" "input" "isinstance"
            "issubclass" "iter" "len" "locals" "map" "max"
            "min" "next" "object" "oct" "open" "ord" "pow"
            "print" "property" "repr" "reversed" "round"
            "setattr" "slice" "sorted" "staticmethod" "sum" "super"
            "type" "vars" "zip" "__import__"))
  "Python built-in functions and types for tree-sitter font-lock.")

(defvar mini-python--treesit-operators
  '("-" "-=" "!=" "*" "**" "**=" "*=" "/" "//" "//=" "/=" "&" "&=" "%" "%="
    "^" "^=" "+" "->" "+=" "<" "<<" "<<=" "<=" "<>" "=" ":=" "==" ">" ">="
    ">>" ">>=" "|" "|=" "~" "@" "@=")
  "Python operators for tree-sitter font-lock.")

;;; Font-lock rules

(defvar mini-python--treesit-settings
  (treesit-font-lock-rules
   ;; Level 1: comment, definition
   :feature 'comment
   :language 'python
   '((comment) @font-lock-comment-face)

   :feature 'definition
   :language 'python
   '((function_definition
      name: (identifier) @font-lock-function-name-face)
     (class_definition
      name: (identifier) @font-lock-type-face)
     (parameters (identifier) @font-lock-variable-name-face)
     (parameters (typed_parameter (identifier) @font-lock-variable-name-face))
     (parameters (default_parameter name: (identifier) @font-lock-variable-name-face)))

   ;; Level 2: keyword, string
   :feature 'keyword
   :language 'python
   `([,@mini-python--treesit-keywords] @font-lock-keyword-face
     ((identifier) @font-lock-keyword-face
      (:match "\\`self\\'" @font-lock-keyword-face)))

   :feature 'string
   :language 'python
   ;; Simplified: all strings use string-face (no docstring distinction)
   '((string) @font-lock-string-face)

   ;; Level 3: builtin, constant, number
   :feature 'builtin
   :language 'python
   `((call function: (identifier) @font-lock-builtin-face
           (:match ,(rx-to-string
                     `(seq bol (or ,@mini-python--treesit-builtins) eol))
                   @font-lock-builtin-face)))

   :feature 'constant
   :language 'python
   '([(true) (false) (none)] @font-lock-constant-face)

   :feature 'number
   :language 'python
   '([(integer) (float)] @font-lock-number-face)

   ;; Level 4: bracket, delimiter, operator
   :feature 'bracket
   :language 'python
   '(["(" ")" "[" "]" "{" "}"] @font-lock-bracket-face)

   :feature 'delimiter
   :language 'python
   '(["," "." ":" ";" (ellipsis)] @font-lock-delimiter-face)

   :feature 'operator
   :language 'python
   `([,@mini-python--treesit-operators] @font-lock-operator-face))
  "Tree-sitter font-lock settings for `mini-python-ts-mode'.")

;;; Major mode definition

;;;###autoload
(define-derived-mode mini-python-ts-mode prog-mode "MiniPy[ts]"
  "Minimal Python major mode using tree-sitter (font-lock only).

This mode uses Emacs's built-in tree-sitter library for syntax
highlighting.  It does not inherit from `python-mode', and does
not configure indentation or imenu.

Requires Emacs 29+ and the tree-sitter Python grammar.
To install the grammar: M-x treesit-install-language-grammar RET python RET

\\{mini-python-ts-mode-map}"
  (if (not (treesit-ready-p 'python))
      (message "mini-python-ts-mode: tree-sitter Python grammar not available. \
Run M-x treesit-install-language-grammar RET python RET first.")
    (treesit-parser-create 'python)
    (setq-local treesit-font-lock-feature-list
                '((comment definition)
                  (keyword string)
                  (builtin constant number)
                  (bracket delimiter operator)))
    (setq-local treesit-font-lock-settings mini-python--treesit-settings)
    (treesit-major-mode-setup)))

(provide 'mini-python-ts-mode)

;;; mini-python-ts-mode.el ends here
