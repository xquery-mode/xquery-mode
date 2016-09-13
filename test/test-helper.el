;;; test-helper.el --- ert-runner test helper

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'cask)

(let ((source-directory (locate-dominating-file load-file-name "Cask")))
  (cask-initialize source-directory)
  (add-to-list 'load-path source-directory))

(undercover "xquery-mode.el")

(setq-default indent-tabs-mode nil)

(require 'xquery-mode)

(setq xquery-mode-indent-style 'native)

(defvar indent-test-count 0)

(defmacro define-indent-test (before after)
  "Generate `ert' test for indentation."
  `(ert-deftest ,(intern (format "test-xquery-mode-indent-%d" (cl-incf indent-test-count))) ()
     (with-current-buffer (generate-new-buffer "*fixture*")
       (insert ,before)
       (goto-char (point-min))
       (xquery-mode)
       (indent-region (point-min) (point-max))
       (should (string= (buffer-substring-no-properties (point-min) (point-max))
                        ,after)))))

(provide 'test-helper)

;;; test-helper.el ends here
