;;; xquery-mode.el --- A simple mode for editing xquery programs

;; Copyright (C) 2005 Suraj Acharya
;; Copyright (C) 2006-2012 Michael Blakeley

;; Authors:
;;   Suraj Acharya <sacharya@cs.indiana.edu>
;;   Michael Blakeley <mike@blakeley.com>
;; URL: https://github.com/xquery-mode/xquery-mode
;; Version: 0.1.0

;; This file is not part of GNU Emacs.

;; xquery-mode.el is free software; you can redistribute it
;; and/or modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

;; TODO: 'if()' is highlighted as a function
;; TODO: requiring nxml-mode excludes XEmacs - just for colors?
;; TODO: test using featurep 'xemacs
;; TODO use nxml for element completion?

(require 'cl-lib)
(require 'thingatpt)
(require 'font-lock)
(require 'nxml-mode)

(defgroup xquery-mode nil
  "Major mode for XQuery files editing."
  :group 'languages)

(defun turn-on-xquery-tab-to-tab-indent ()
  "Turn on tab-to-tab XQuery-mode indentation."
  (define-key xquery-mode-map (kbd "TAB") 'tab-to-tab-stop)
  (kill-local-variable 'indent-line-function)
  (kill-local-variable 'indent-region-function))

(defun turn-on-xquery-native-indent ()
  "Turn on native XQuery-mode indentation."
  (define-key xquery-mode-map (kbd "TAB") 'indent-for-tab-command)
  (set (make-local-variable 'indent-line-function) 'xquery-mode-indent-line)
  (set (make-local-variable 'indent-region-function) 'xquery-mode-indent-region))

(defun toggle-xquery-mode-indent-style ()
  "Switch to the next indentation style."
  (interactive)
  (if (eq xquery-mode-indent-style 'tab-to-tab)
      (setq xquery-mode-indent-style 'native)
    (setq xquery-mode-indent-style 'tab-to-tab))
  (xquery-mode-activate-indent-style))

(defun xquery-mode-activate-indent-style ()
  "Activate current indentation style."
  (cond ((eq xquery-mode-indent-style 'tab-to-tab)
         (turn-on-xquery-tab-to-tab-indent))
        ((eq xquery-mode-indent-style 'native)
         (turn-on-xquery-native-indent))))

(defcustom xquery-mode-hook nil
  "Hook run after entering XQuery mode."
  :type 'hook
  :options '(turn-on-font-lock))

(defvar xquery-toplevel-bovine-table nil
  "Top level bovinator table.")

(defvar xquery-mode-syntax-table ()
  "Syntax table for xquery-mode.")

(setq xquery-mode-syntax-table
      (let ((xquery-mode-syntax-table (make-syntax-table)))
        ;; single-quotes are equivalent to double-quotes
        (modify-syntax-entry ?' "\"" xquery-mode-syntax-table)
        ;; treat underscores as punctuation
        (modify-syntax-entry ?\_ "." xquery-mode-syntax-table)
        ;; treat hypens as punctuation
        (modify-syntax-entry ?\- "." xquery-mode-syntax-table)
        ;; colons are both punctuation and comments
        ;; the space after '.' indicates an unused matching character slot
        (modify-syntax-entry ?\: ". 23" xquery-mode-syntax-table)
        ;; XPath step separator / is punctuation
        (modify-syntax-entry ?/ "." xquery-mode-syntax-table)
        ;; xquery doesn't use backslash-escaping, so \ is punctuation
        (modify-syntax-entry ?\\ "." xquery-mode-syntax-table)
        ;; set-up the syntax table correctly for all the different braces
        (modify-syntax-entry ?\{ "(}" xquery-mode-syntax-table)
        (modify-syntax-entry ?\} "){" xquery-mode-syntax-table)
        (modify-syntax-entry ?\[ "(]" xquery-mode-syntax-table)
        (modify-syntax-entry ?\] ")]" xquery-mode-syntax-table)
        ;; parens may indicate a comment, or may be a sequence
        (modify-syntax-entry ?\( "()1n" xquery-mode-syntax-table)
        (modify-syntax-entry ?\) ")(4n" xquery-mode-syntax-table)
        xquery-mode-syntax-table))

(defvar xquery-mode-keywords ()
  "Keywords for xquery-mode.")

(defvar xquery-mode-comment-start "(: "
  "String used to start an XQuery mode comment.")

(defvar xquery-mode-comment-end " :)"
  "String used to end an XQuery mode comment.")

(defvar xquery-mode-comment-fill ":"
  "String used to fill an XQuery mode comment.")

(defvar xquery-mode-comment-start-skip "(:\\s-+"
  "Regexp to match an XQuery mode comment and any following whitespace.")

;;;###autoload
(define-derived-mode xquery-mode fundamental-mode "XQuery"
  "A major mode for W3C XQuery 1.0"
  ;; indentation
  (setq nxml-prolog-end (point-min))
  (setq nxml-scan-end (copy-marker (point-min) nil))
  (make-local-variable 'forward-sexp-function)
  (setq forward-sexp-function 'xquery-forward-sexp)
  (local-set-key "/" 'nxml-electric-slash)
  (setq tab-width xquery-mode-indent-width)
  (xquery-mode-activate-indent-style)
  ;; apparently it's important to set at least an empty list up-front
  (set (make-local-variable 'font-lock-defaults) '((nil)))
  (set (make-local-variable 'comment-start) xquery-mode-comment-start)
  (set (make-local-variable 'comment-end) xquery-mode-comment-end)
  (set (make-local-variable 'comment-fill)  xquery-mode-comment-fill)
  (set (make-local-variable 'comment-start-skip) xquery-mode-comment-start-skip))

;; TODO: move it upper.
(defcustom xquery-mode-indent-style 'tab-to-tab
  "Indentation behavior.
`tab-to-tab' to use `tab-to-tab-stop' indent function
`native' to use own indentation engine"
  :group 'xquery-mode
  :type '(choice (const :tag "Tab to tab" tab-to-tab)
                 (const :tag "Native" native))
  :set (lambda (var key)
         (set var key)
         (xquery-mode-activate-indent-style)))

(defcustom xquery-mode-indent-width 2
  "Indent width for `xquery-mode'."
  :group 'xquery-mode
  :type 'integer)

;; XQuery doesn't have keywords, but these usually work...
;; TODO: remove as many as possible, in favor of parsing
(setq xquery-mode-keywords
      (list
       ;; FLWOR
       ;;"let" "for"
       "at" "in"
       "where"
       "stable order by" "order by"
       "ascending" "descending" "empty" "greatest" "least" "collation"
       "return"
       ;; XPath axes
       "self" "child" "descendant" "descendant-or-self"
       "parent" "ancestor" "ancestor-or-self"
       "following" "following-sibling"
       "preceding" "preceding-sibling"
       ;; conditionals
       "if" "then" "else"
       "typeswitch" ;"case" "default"
       ;; quantified expressions
       "some" "every" "construction" "satisfies"
       ;; schema
       "schema-element" "schema-attribute" "validate"
       ;; operators
       "intersect" "union" "except" "to"
       "is" "eq" "ne" "gt" "ge" "lt" "le"
       "or" "and"
       "div" "idiv" "mod"))

(defvar xquery-mode-keywords-regex
  (concat "\\b\\("
          (mapconcat
           (function
            (lambda (r)
              (if (string-match "[ \t]+" r)
                  (replace-match "[ \t]+" nil t r)
                r)))
           xquery-mode-keywords
           "\\|")
          "\\)\\b")
  "Keywords regex for xquery mode.")

;; XQuery syntax - TODO build a real parser
(defvar xquery-mode-ncname "\\(\\sw[-_\\.[:word:]]*\\)"
  "NCName regex, in 1 group.")

;; highlighting needs a group, even if it's "" - so use (...?) not (...)?
;; note that this technique treats the local-name as optional,
;; when the prefix should be the optional part.
(defvar xquery-mode-qname
  (concat
   xquery-mode-ncname
   "\\(:?\\)"
   "\\("
   xquery-mode-ncname
   "?\\)")
  "QName regex, in 3 groups.")

(font-lock-add-keywords
 'xquery-mode
 `(
   ;; prolog version decl
   ("\\(xquery\\s-+version\\)\\s-+"
    (1 font-lock-keyword-face))
   ;; namespace default decl for 0.9 or 1.0
   (,(concat
      "\\(\\(declare\\)?"
      "\\(\\s-+default\\s-+\\(function\\|element\\)\\)"
      "\\s-+namespace\\)\\s-+")
    (1 font-lock-keyword-face))
   ;; namespace decl
   (,(concat
      "\\(declare\\s-+namespace\\)\\s-+")
    (1 font-lock-keyword-face))
   ;; option decl
   (,(concat "\\(declare\\s-+option\\s-+" xquery-mode-qname "\\)")
    (1 font-lock-keyword-face))
   ;; import module decl - must precede library module decl
   ("\\(import\\s-+module\\)\\s-+\\(namespace\\)?\\s-+"
    (1 font-lock-keyword-face)
    (2 font-lock-keyword-face))
   ;; library module decl, for 1.0 or 0.9-ml
   ("\\(module\\)\\s-+\\(namespace\\)?\\s-*"
    (1 font-lock-keyword-face)
    (2 font-lock-keyword-face))
   ;; import schema decl
   ("\\(import\\s-+schema\\)\\s-+\\(namespace\\)?\\s-+"
    (1 font-lock-keyword-face)
    (2 font-lock-keyword-face))
   ;; variable decl
   ("\\(for\\|let\\|declare\\s-+variable\\|define\\s-+variable\\)\\s-+\\$"
    (1 font-lock-keyword-face))
   ;; variable name
   (,(concat "\\($" xquery-mode-qname "\\)")
    (1 font-lock-variable-name-face))
   ;; function decl
   (,(concat
      "\\(declare\\s-+function\\"
      "|declare\\s-+private\\s-+function\\"
      "|define\\s-+function\\)\\s-+\\("
      xquery-mode-qname "\\)(")
    (1 font-lock-keyword-face)
    (2 font-lock-function-name-face))
   ;; schema test or type decl
   (,(concat
      "\\("
      "case"
      "\\|instance\\s-+of\\|castable\\s-+as\\|treat\\s-+as\\|cast\\s-+as"
      ;; "as" must be last in the list
      "\\|as"
      "\\)"
      "\\s-+\\(" xquery-mode-qname "\\)"
      ;; type may be followed by element() or element(x:foo)
      "(?\\s-*\\(" xquery-mode-qname "\\)?\\s-*)?")
    (1 font-lock-keyword-face)
    (2 font-lock-type-face)
    ;; TODO: the second qname never matches
    (3 font-lock-type-face))
   ;; function call
   (,(concat "\\(" xquery-mode-qname "\\)(")
    (1 font-lock-function-name-face))
   ;; named node constructor
   (,(concat "\\(attribute\\|element\\)\\s-+\\(" xquery-mode-qname "\\)\\s-*{")
    (1 font-lock-keyword-face)
    (2 font-lock-constant-face))
   ;; anonymous node constructor
   ("\\(binary\\|comment\\|document\\|text\\)\\s-*{"
    (1 font-lock-keyword-face))
   ;; typeswitch default
   ("\\(default\\s-+return\\)\\s-+"
    (1 font-lock-keyword-face)
    (2 font-lock-keyword-face))
   ;; xml start element start
   (,(concat "<" xquery-mode-qname)
    (1 'nxml-element-prefix)
    (2 'nxml-element-colon)
    (3 'nxml-element-prefix))
   ;; xml start element end
   ("\\(/?\\)>"
    (1 'nxml-tag-slash))
   ;; xml end element
   (,(concat "<\\(/\\)" xquery-mode-qname ">")
    (1 'nxml-tag-slash)
    (2 'nxml-element-prefix)
    (3 'nxml-element-colon)
    (4 'nxml-element-local-name))
   ;; TODO: xml attribute or xmlns decl
   ;; xml comments
   ("\\(<!--\\)\\([^-]*\\)\\(-->\\)"
    (1 'nxml-comment-delimiter)
    (2 'nxml-comment-content)
    (3 'nxml-comment-delimiter))
   ;; highlighting XPath expressions, including *:foo
   ;;
   ;; TODO: this doesn't match expressions unless they start with
   ;; slash
   ;;
   ;; TODO: but matching without a leading slash overrides all the
   ;; keywords
   (,(concat "\\(//?\\)\\(*\\|\\sw*\\)\\(:?\\)" xquery-mode-ncname)
    (1 font-lock-constant-face)
    (2 font-lock-constant-face)
    (3 font-lock-constant-face)
    (4 font-lock-constant-face))
   ;; highlighting pseudo-keywords - must be late, for problems like
   ;; 'if ()'
   (,xquery-mode-keywords-regex
    (1 font-lock-keyword-face))))

;;;###autoload
(add-to-list 'auto-mode-alist '(".xq[erxy]\\'" . xquery-mode))

(defun xquery-forward-sexp (&optional arg)
  "XQuery forward s-expresssion.
This function is not very smart.  It tries to use
`nxml-forward-balanced-item' if it sees '>' or '<' characters in
the current line (ARG), and uses the regular `forward-sexp'
otherwise."
  (if (> arg 0)
      (progn
        (if (looking-at "\\s-*<")
            (nxml-forward-balanced-item arg)
          (let ((forward-sexp-function nil)) (forward-sexp arg))))
    (if (looking-back ">\\s-*")
        (nxml-forward-balanced-item arg)
      (let ((forward-sexp-function nil)) (forward-sexp arg)))))

(defun xquery-mode-indent-line ()
  "Indent current line as xquery code."
  (xquery-mode-indent-region
   (line-beginning-position)
   (line-end-position)))

(defun xquery-mode-indent-region (start end)
  "Indent given region.
START and END are region boundaries."
  (interactive "r")
  (save-excursion
    (let* ((literals '(("\\(?:\\<declare\\>\\|\\<define\\>\\)\\(?:\\s-+\\<private\\>\\)?\\s-+\\<function\\>.*(\\s-*$" . function-name-stmt)
                       ("\\s-+\\<function\\>.*(\\s-*$" . function-name-stmt)
                       ("\\<declare\\>\\s-+\\<variable\\>" . declare-variable-stmt)
                       ("\\<module\\>\\s-+\\<namespace\\>" . namespace-stmt)
                       ("\\<import\\>\\s-+\\<module\\>" . import-stmt)
                       ("(:" . comment-start-stmt)
                       (":)" . comment-end-stmt)
                       ("{" . open-curly-bracket-stmt)
                       ("}" . close-curly-bracket-stmt)
                       ("(" . open-round-bracket-stmt)
                       (")" . close-round-bracket-stmt)
                       ("<[^>/ ]+?\\>[^>]*>" . open-xml-tag-stmt)
                       ("</[^>]+>" . close-xml-tag-stmt)
                       ("\\\\\"" . escaped-double-quote-stmt)
                       ("\\\\'" . escaped-quote-stmt)
                       ("\"" . double-quote-stmt)
                       ("'" . quote-stmt)
                       ("\"" . close-double-quote-stmt) ;; Don't ask...
                       ("'" . close-quote-stmt)
                       ("\\<for\\>" . for-stmt)
                       ("\\<let\\>" . let-stmt)
                       ("\\<order\\>\\s-+\\<by\\>" . order-by-stmt)
                       (":=" . assign-stmt)
                       (";" . semicolon-stmt)
                       (":" . colon-stmt)
                       ("\\<if\\>" . if-stmt)
                       ("\\<then\\>" . then-stmt)
                       ("\\<else\\>" . else-stmt)
                       ("\\<where\\>" . where-stmt)
                       ("\\<return\\>" . return-stmt)
                       ("\\<typeswitch\\>" . typeswitch-stmt)
                       ("\\<case\\>" . case-stmt)
                       ("\\<default\\>" . default-stmt)
                       ("\\<element\\>" . element-stmt)
                       ("\\$\\(?:[[:alnum:]-_.:/]\\|\\[\\|\\]\\)+" . var-stmt)
                       ("\\(?:[[:alnum:]-_.:/]\\|\\[\\|\\]\\)+" . word-stmt)))
           (lookup-expression-fn (lambda (stream line-stream found-literal offset)
                                   (when (memq (caar (append line-stream stream))
                                               '(where-stmt open-curly-bracket-stmt then-stmt else-stmt))
                                     (list 'expression-start-stmt offset))))
           (substitutions (list (list 'var-stmt
                                      lookup-expression-fn 'var-stmt)
                                (list 'word-stmt
                                      lookup-expression-fn 'word-stmt 'element-arg-end-stmt)
                                '(element-stmt
                                  element-stmt element-arg-stmt)
                                '(where-stmt
                                  expression-end-stmt where-stmt)
                                '(return-stmt
                                  expression-end-stmt return-stmt)
                                '(close-curly-bracket-stmt
                                  expression-end-stmt close-curly-bracket-stmt)
                                '(close-round-bracket-stmt
                                  expression-end-stmt close-round-bracket-stmt element-arg-end-stmt element-end-stmt)
                                '(else-stmt
                                  expression-end-stmt else-stmt)
                                '(let-stmt
                                  expression-end-stmt let-stmt)))
           (on-close '((expression-start-stmt . expression-stmt)
                       (element-stmt . expression-stmt)
                       (open-curly-bracket-stmt . expression-stmt)
                       (open-round-bracket-stmt . expression-stmt)
                       (open-xml-tag-stmt . expression-stmt)
                       (else-stmt . expression-stmt)
                       (double-quote-stmt . expression-stmt)
                       (quote-stmt . expression-stmt)
                       (typeswitch-stmt . expression-stmt)
                       ;; TODO: surround expr-start for return expr-end
                       ;; TODO: surround expr-start if then else expr-end
                       (return-stmt . expression-stmt)))
           ;; TODO: assign-stmt should be closed by strings and numbers.
           (opposite '((close-curly-bracket-stmt open-curly-bracket-stmt)
                       (close-round-bracket-stmt open-round-bracket-stmt function-name-stmt)
                       (close-xml-tag-stmt open-xml-tag-stmt)
                       (close-double-quote-stmt double-quote-stmt)
                       (close-quote-stmt quote-stmt)
                       (assign-stmt let-stmt declare-variable-stmt)
                       (return-stmt where-stmt for-stmt)
                       (for-stmt for-stmt)
                       (where-stmt for-stmt)
                       (else-stmt if-stmt)
                       (default-stmt typeswitch-stmt)
                       (semicolon-stmt namespace-stmt import-stmt assign-stmt)
                       (comment-end-stmt comment-start-stmt)
                       (expression-stmt return-stmt else-stmt assign-stmt element-stmt element-arg-stmt)
                       (element-end-stmt element-stmt)
                       (element-arg-end-stmt element-arg-stmt)
                       (expression-end-stmt expression-start-stmt)
                       (var-stmt assign-stmt return-stmt)))
           (implicit-statements '(expression-end-stmt expression-stmt))
           (next-re-table '((comment-start-stmt . inside-comment)
                            (comment-end-stmt . generic)
                            (double-quote-stmt . inside-double-quoted-string)
                            (close-double-quote-stmt . generic)
                            (quote-stmt . inside-string)
                            (close-quote-stmt . generic)))
           (grid (list (cons 'generic
                             (mapcar #'cdr literals))
                       '(inside-comment
                         comment-end-stmt colon-stmt word-stmt)
                       '(inside-double-quoted-string
                         escaped-double-quote-stmt close-double-quote-stmt word-stmt)
                       '(inside-string
                         escaped-quote-stmt close-quote-stmt word-stmt)))
           ;; TODO: don't calculate indent pairs.  Write it declarative way.
           ;; TODO: make variable below calculated only.
           (non-pairs '(comment-end-stmt var-stmt word-stmt assign-stmt))
           ;; TODO: This duplication makes me sad very often.
           (pairs '((close-curly-bracket-stmt open-curly-bracket-stmt)
                    (close-round-bracket-stmt open-round-bracket-stmt function-name-stmt)))
           ;; TODO: that's not good at all.
           (aligned-pairs (append (cl-remove-if (lambda (x) (member x (append non-pairs (mapcar #'car pairs))))
                                                opposite
                                                :key #'car)
                                  '((then-stmt if-stmt)
                                    (let-stmt for-stmt)
                                    (order-by-stmt for-stmt)
                                    (case-stmt typeswitch-stmt)
                                    (comment-start-stmt typeswitch-stmt))))
           (opening (apply #'append (mapcar #'cdr opposite)))
           (closing (mapcar #'car opposite))
           (re-table (mapcar (lambda (g)
                               (let* ((name (car g))
                                      (grid-literals (cdr g))
                                      (group-lookup (cl-loop for x in grid-literals
                                                             for y from 1
                                                             collect (cons y x)))
                                      (groups (mapcar #'car group-lookup))
                                      (re (mapconcat (lambda (x) (concat "\\(" (car x) "\\)"))
                                                     (cl-remove-if-not
                                                      (lambda (x) (memq (cdr x) grid-literals))
                                                      literals)
                                                     "\\|")))
                                 (list name re groups group-lookup)))
                             grid))
           (current-indent 0)
           (re (cadr (assoc 'generic re-table)))
           (groups (cl-caddr (assoc 'generic re-table)))
           (group-lookup (cl-cadddr (assoc 'generic re-table)))
           stream
           line-stream
           align-column
           exit)
      (goto-char start)
      (if (eq (forward-line -1) -1)
          (setq align-column 0)
        (setq align-column (current-indentation)))
      (push (list 'buffer-beginning align-column 0) stream)
      (while (not exit)
        (if (not (re-search-forward re (line-end-position) t))
            (progn
              (push (list 'newline-stmt (line-end-position)) line-stream)
              (setq line-stream (reverse line-stream))
              (while (memq (caar line-stream) implicit-statements)
                (let ((token (car (pop line-stream))))
                  (when (and (memq token closing)
                             (memq (caar stream)
                                   (cdr (assoc token opposite))))
                    (let* ((closed (car (pop stream)))
                           (trigger (cdr (assoc closed on-close))))
                      ;; TODO: move this to the search part.
                      (when trigger
                        (push (list trigger nil) line-stream))))))
              (cl-destructuring-bind (previous-token previous-indent previous-offset)
                  (car stream)
                (cond
                 ((and (eq previous-token 'comment-start-stmt)
                       (memq (caar line-stream) '(colon-stmt comment-end-stmt)))
                  (setq current-indent (+ previous-indent previous-offset 1)))
                 ((eq previous-token 'comment-start-stmt)
                  (setq current-indent (+ previous-indent previous-offset 3)))
                 ((cl-loop for pair in pairs
                           thereis (and (eq (caar line-stream) (car pair))
                                        (memq previous-token (cdr pair))))
                  (setq current-indent previous-indent))
                 ((cl-loop for pair in aligned-pairs
                           thereis (and (eq (caar line-stream) (car pair))
                                        (memq previous-token (cdr pair))))
                  (setq current-indent (+ previous-indent previous-offset)))
                 ((eq previous-token 'open-round-bracket-stmt)
                  (setq current-indent (+ previous-indent previous-offset 1)))
                 ((memq previous-token '(open-curly-bracket-stmt assign-stmt))
                  (setq current-indent (+ previous-indent xquery-mode-indent-width)))
                 ((memq previous-token '(open-xml-tag-stmt
                                         return-stmt if-stmt else-stmt where-stmt
                                         let-stmt declare-variable-stmt
                                         namespace-stmt import-stmt
                                         function-name-stmt typeswitch-stmt))
                  (setq current-indent (+ previous-indent previous-offset xquery-mode-indent-width)))
                 ((eq previous-token 'expression-start-stmt)
                  (setq current-indent (+ previous-indent previous-offset)))
                 ((eq previous-token 'buffer-beginning)
                  (setq current-indent previous-indent))))
              (when (and (<= start (point))
                         (<= (point) end))
                (setq end (+ end (- current-indent (current-indentation))))
                (indent-line-to current-indent))
              (if (>= (line-end-position) end)
                  (setq exit t)
                (while line-stream
                  (let ((token (pop line-stream)))
                    (cl-destructuring-bind (current-token current-offset)
                        token
                      (when (and (memq current-token closing)
                                 (memq (caar stream)
                                       (cdr (assoc current-token opposite))))
                        (let* ((closed (car (pop stream)))
                               (trigger (cdr (assoc closed on-close))))
                          ;; TODO: move this to the search part.
                          (when trigger
                            (push (list trigger current-offset) line-stream))))
                      (when (memq current-token opening)
                        (push (list current-token current-indent current-offset) stream)))))
                (forward-line)
                (beginning-of-line)))
          (let* ((matched-group (cl-find-if #'match-string-no-properties groups))
                 (found-literal (cdr (assoc matched-group group-lookup)))
                 (offset (- (current-column)
                            (current-indentation)
                            (length (match-string-no-properties 0)))))
            (let ((substitution (cdr (assoc found-literal substitutions))))
              (if substitution
                  (dolist (s substitution)
                    (if (symbolp s)
                        (push (list s offset) line-stream)
                      (let ((result (apply s stream line-stream found-literal offset nil)))
                        (when result
                          (push result line-stream)))))
                (push (list found-literal offset) line-stream)))
            (let ((next-re-key (cdr (assoc found-literal next-re-table))))
              (when next-re-key
                (cl-destructuring-bind (next-re-re next-re-groups next-re-lookups)
                    (cdr (assoc next-re-key re-table))
                  (setq re next-re-re
                        groups next-re-groups
                        group-lookup next-re-lookups))))))))))

(provide 'xquery-mode)

;;; xquery-mode.el ends here
