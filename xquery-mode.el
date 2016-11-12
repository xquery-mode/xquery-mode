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
  (kill-local-variable 'indent-line-function))

(defun turn-on-xquery-native-indent ()
  "Turn on native XQuery-mode indentation."
  (define-key xquery-mode-map (kbd "TAB") 'indent-for-tab-command)
  (set (make-local-variable 'indent-line-function) 'xquery-indent-line))

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

(defun xquery-indent-line ()
  "Indent current line as xquery code."
  (interactive)
  (let ((savept (> (current-column) (current-indentation)))
        (indent (or (xquery-calculate-indentation) 0)))
    (save-excursion
      (back-to-indentation)
      (when (string-match-p
             "\t" (buffer-substring-no-properties
                   (line-beginning-position)
                   (point)))
        (delete-region (line-beginning-position) (point)))
      (indent-line-to indent))
    (when (not savept)
      (back-to-indentation))))

(defun xquery-calculate-indentation ()
  "Calculate the indentation for a line of XQuery.
This function returns the column to which the current line should
be indented."
  (cond
   ((or (looking-back "\\`\\(\\s-*\\|\n*\\)*" nil)
        (line-starts-with "\\'")
        (looking-at-p "^[[:space:]]*$"))
    0)
   ((and (line-starts-with "\\<at\\>\\s-+")
         (or (previous-line-starts-with"\\<import\\>\\s-+\\<module\\>\\s-+")
             (previous-line-starts-with "\\(\\<define\\>\\|\\<declare\\>\\)\\s-+\\<function\\>\\s-+")))
    (+ (previous-line-indentation) xquery-mode-indent-width))
   ((and (line-starts-with "=")
         (previous-line-starts-with "\\<module\\>\\s-+\\<namespace\\>\\s-+"))
    (+ (previous-line-indentation) xquery-mode-indent-width))
   ((and (or (previous-line-starts-with "\\<at\\>\\s-+")
             (previous-line-starts-with "="))
         (previous-line-ends-with ";"))
    (- (previous-line-indentation) xquery-mode-indent-width))
   ((and (previous-line-starts-with "\\(\\<define\\>\\|\\<declare\\>\\)\\s-+\\<function\\>\\s-+")
         (line-starts-with "{"))
    (previous-line-indentation))
   ((line-starts-with ")")
    (search-backward-unclosed
     "(" ")"
     :func #'(lambda ()
               (if (looking-back "\\(\\<define\\>\\|\\<declare\\>\\)\\s-+\\<function\\>\\s-+.*"
                                 (line-beginning-position))
                   (current-indentation)
                 (current-column)))))
   ((line-starts-with "}")
    (search-backward-unclosed "{" "}" :func #'current-indentation))
   ((line-starts-with "</\\([^>]+\\)>")
    (let ((tag (match-string-no-properties 1)))
      (search-backward-unclosed (format "<%s\\>[^>]*>" tag) (format "</%s>" tag))))
   ;; TODO: open xml comment
   ;; TODO: close xml comment
   ((line-starts-with "\\(\\<default\\|\\<case\\>\\)")
    (save-excursion
      (re-search-backward "\\<typeswitch\\>")
      (current-column)))
   ((previous-line-starts-with "\\<case\\>")
    (+ (previous-line-indentation) xquery-mode-indent-width))
   ((and (line-starts-with "\\(\\<return\\>\\|\\<let\\>\\|\\<where\\>\\|\\<order\\>\\s-+\\<by\\>\\)")
         (save-excursion
           (let (exit result)
             (while (not exit)
               (if (re-search-backward "\\<for\\>\\|\\<let\\>\\|\\<where\\>\\|\\<order\\>\\s-+\\<by\\>" nil t)
                   (when (not (nth 8 (syntax-ppss)))
                     (setq exit t result t))
                 (setq exit t)))
             result)))
    (save-excursion
      ;; TODO: remove this duplication
      (let (exit result)
        (while (not exit)
          (if (re-search-backward "\\<for\\>\\|\\<let\\>\\|\\<where\\>\\|\\<order\\>\\s-+\\<by\\>" nil t)
              (when (not (nth 8 (syntax-ppss)))
                (setq exit t result t))
            (setq exit t)))
        result)
      (current-column)))
   ((previous-line-starts-with "\\<return\\>\\s-*$")
    (+ (previous-line-indentation) xquery-mode-indent-width))
   ((line-starts-with "\\<else\\>")
    (search-backward-unclosed "\\<if\\>" "\\<else\\>"))
   ((line-starts-with "\\<then\\>")
    (save-excursion
      (re-search-backward "\\<if\\>")
      (current-column)))
   ((previous-line-ends-with "\\<else\\>")
    (+ (previous-line-indentation) xquery-mode-indent-width))
   ((previous-line-ends-with "\\<then\\>")
    (save-excursion
      (re-search-backward "\\<if\\>")
      (+ (current-column) xquery-mode-indent-width)))
   ((previous-line-starts-with "\\<if\\>")
    (previous-line-indentation))
   ;; TODO: any logical operator
   ;; TODO: previous line ends with logical operator
   ((and (line-starts-with "\\(=>\\|\\<or\\>\\)")
         (not (previous-line-starts-with "\\(=>\\|\\<or\\>\\)")))
    (save-excursion
      (forward-line -1)
      (end-of-line)
      (let ((tokens (reverse (split-string (thing-at-point 'line))))
            tokens-to-skip)
        (push (pop tokens) tokens-to-skip)
        (while (member (car tokens) '("!=" "or"))
          (dotimes (i 2)
            (push (pop tokens) tokens-to-skip)))
        (re-search-backward (mapconcat #'identity tokens-to-skip "\\s-+")
                            (line-beginning-position))
        (current-column))))
   ((and (line-starts-with "\\(=>\\|\\<or\\>\\)")
         (previous-line-starts-with "\\(=>\\|\\<or\\>\\)"))
    (previous-line-indentation))
   ((previous-line-ends-with ":=")
    (+ (previous-line-indentation) xquery-mode-indent-width))
   ((search-backward-unclosed "(:" ":)")
    (if (line-starts-with ":")
        (1+ (search-backward-unclosed "(:" ":)"))
      (+ 3 (search-backward-unclosed "(:" ":)"))))
   ((previous-line-ends-with ":)")
    (save-excursion
      (re-search-backward ":)")
      (search-backward-unclosed "(:" ":)" :func #'current-indentation)))
   ;; ((and (line-starts-with "(:")
   ;;       (save-excursion
   ;;         (re-search-forward ":)" nil t)))
   ;;  (save-excursion
   ;;    (re-search-forward ":)")
   ;;    (forward-line)
   ;;    (beginning-of-line)
   ;;    (xquery-calculate-indentation)))
   ((search-backward-first-unclosed)
    (save-excursion
      (goto-char (search-backward-first-unclosed))
      (cond
       ((and (looking-at-p "(")
             (looking-back "\\(\\<define\\>\\|\\<declare\\>\\)\\s-+\\<function\\>\\s-+.*"
                           (line-beginning-position)))
        (+ (current-indentation) xquery-mode-indent-width))
       ((looking-at-p "(")
        (1+ (current-column)))
       ((looking-at-p "<")
        (+ (current-column) xquery-mode-indent-width))
       ((looking-at-p "{\\s-*$")
        (+ (current-indentation) xquery-mode-indent-width))
       ((looking-at-p "{")
        (1+ (current-column))))))
   (t (previous-line-indentation))))

(defun line-starts-with (re)
  (save-excursion
    (beginning-of-line)
    (looking-at (concat "^\\s-*" re))))

(defun line-ends-with (re)
  (save-excursion
    (end-of-line)
    (looking-back (concat re "\\s-*$"))))

(defun previous-line-starts-with (re)
  (save-excursion
    (forward-line -1)
    ;; FIXME: beginning of buffer reached
    (while (string-match-p "\\`\\s-*\n?\\'" (thing-at-point 'line))
      (forward-line -1))
    (line-starts-with re)))

(defun previous-line-ends-with (re)
  (save-excursion
    (forward-line -1)
    (while (string-match-p "\\`\\s-*\n?\\'" (thing-at-point 'line))
      (forward-line -1))
    (line-ends-with re)))

(defun previous-line-indentation ()
  (save-excursion
    (forward-line -1)
    (while (string-match-p "\\`\\s-*\n?\\'" (thing-at-point 'line))
      (forward-line -1))
    (current-indentation)))

(defun previous-line-beginning-possition ()
  (save-excursion
    (forward-line -1)
    (while (string-match-p "\\`\\s-*\n?\\'" (thing-at-point 'line))
      (forward-line -1))
    (line-beginning-position)))

(defun previous-line-end-possition ()
  (save-excursion
    (forward-line -1)
    (while (string-match-p "\\`\\s-*\n?\\'" (thing-at-point 'line))
      (forward-line -1))
    (line-end-position)))

(cl-defun search-backward-unclosed (open closing &key (func #'current-column))
  (save-excursion
    (let ((close-counter 0)
          exit result)
      (while (not exit)
        (let ((pos (re-search-backward (concat open "\\|" closing) nil t)))
          (cond ((not pos)
                 (setq exit t))
                ((looking-at-p closing)
                 (cl-incf close-counter))
                ((looking-at-p open)
                 (if (zerop close-counter)
                     (setq exit t
                           result (apply func nil))
                   (cl-decf close-counter))))))
      result)))

(defun search-backward-first-unclosed ()
  (let ((matching '(("(" . ")") ("{" . "}")))
        tags)
    (save-excursion
      (while (re-search-backward "(\\|{\\|<[^/> \t]+\\>" nil t)
        (let* ((open (match-string-no-properties 0))
               (closing (cdr (assoc open matching))))
          (unless closing
            (setq open (concat open "[^>]*>")
                  closing (concat "</" (substring open 1))))
          (push (cons open closing) tags))))
    (car (sort (delq nil
                     (mapcar (lambda (x)
                               (cl-destructuring-bind (open . closing) x
                                 (search-backward-unclosed open closing :func #'point)))
                             tags))
               #'>))))

(defun xquery-mode-indent-region (start end)
  "Indent given region.
START and END are region boundaries."
  (interactive "r")
  (save-excursion
    (let* ((literals '(("{$" . open-curly-bracket-at-the-end)
                       ("{" . open-curly-bracket)
                       ("}" . close-curly-bracket)
                       ("(" . open-round-bracket)
                       (")" . close-round-bracket)
                       ("<[^>/ ]+?\\>[^>]*>" . open-xml-tag)
                       ("</[^>]+>" . close-xml-tag)
                       ("\\<if\\>" . if-stmt)
                       ("\\<then\\>" . then-stmt)
                       ("\\<else\\>" . else-stmt)
                       ("\\<return\\>" . return-stmt)
                       ("\\<[^[:space:]]+?\\>" . word-stmt)))
           (opposite '((close-curly-bracket open-curly-bracket-at-the-end open-curly-bracket)
                       (close-round-bracket open-round-bracket)
                       (close-xml-tag open-xml-tag)
                       (else-stmt if-stmt)
                       (terminator-stmt return-stmt else-stmt)))
           (pairs (append opposite
                          '((then-stmt if-stmt))))
           (terminators '(close-curly-bracket
                          close-round-bracket
                          close-xml-tag))
           (opening (apply #'append (mapcar #'cdr opposite)))
           (closing (mapcar #'car opposite))
           (group-lookup (cl-loop for x in literals
                                  for y from 1
                                  collect (cons y (cdr x))))
           (groups (mapcar #'car group-lookup))
           (re (mapconcat (lambda (x) (concat "\\(" (car x) "\\)"))
                          literals
                          "\\|"))
           (current-indent 0)
           stream line-stream exit)
      (goto-char (point-min))
      (while (not exit)
        (if (not (re-search-forward re (line-end-position) t))
            (progn
              (and stream line-stream
                   (cl-destructuring-bind (previous-token previous-indent previous-offset)
                       (car stream)
                     (cl-destructuring-bind (first-token first-indent first-offset)
                         (car (last line-stream))
                       (cond
                        ((cl-loop for pair in pairs
                                  thereis (and (eq first-token (car pair))
                                               (memq previous-token (cdr pair))))
                         (setq current-indent (+ previous-indent previous-offset)))
                        ((memq previous-token '(open-curly-bracket open-round-bracket))
                         (setq current-indent (+ previous-indent previous-offset 1)))
                        ((eq previous-token 'open-curly-bracket-at-the-end)
                         (setq current-indent (+ previous-indent xquery-mode-indent-width)))
                        ((memq previous-token '(open-xml-tag return-stmt if-stmt else-stmt))
                         (setq current-indent (+ previous-indent previous-offset xquery-mode-indent-width)))))))
              (indent-line-to current-indent)
              (if (eq (line-end-position) (point-max))
                  (setq exit t)
                (dolist (token (reverse line-stream))
                  (cl-destructuring-bind (current-token nil current-offset)
                      token
                    (when (and (memq current-token closing)
                               (memq (caar stream)
                                     (cdr (assoc current-token opposite))))
                      (pop stream))
                    (when (memq current-token opening)
                      (push (list current-token current-indent current-offset) stream))))
                (setq line-stream nil)
                (forward-line)
                (beginning-of-line)))
          (let* ((matched-group (cl-find-if #'match-string-no-properties groups))
                 (found-literal (cdr (assoc matched-group group-lookup)))
                 (terminator-offset (- (current-column) (current-indentation)))
                 (offset (- terminator-offset (length (match-string-no-properties 0)))))
            (push (list found-literal nil offset) line-stream)
            (when (memq found-literal terminators)
              (push (list 'terminator-stmt nil terminator-offset) line-stream))))))))

(provide 'xquery-mode)

;;; xquery-mode.el ends here
