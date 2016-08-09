;;; test-helper.el --- ert-runner test helper

;;; Commentary:

;;; Code:

(require 'cask)
(require 'shut-up)

(let ((source-directory (locate-dominating-file load-file-name "Cask")))
  (cask-initialize source-directory)
  (add-to-list 'load-path source-directory))

(undercover "xquery-mode.el")

(require 'xquery-mode)

(defmacro define-indent-test (testname args doc before after)
  "Generate `ert' test for indentation."
  (declare (indent 2))
  `(ert-deftest ,(intern (concat "test-xquery-mode-" (symbol-name testname))) ,args
     ,doc
     (with-current-buffer (generate-new-buffer "*fixture*")
       (insert ,before)
       (goto-char (point-min))
       (xquery-mode)
       (shut-up (indent-region (point-min) (point-max)))
       (should (string= (buffer-substring-no-properties (point-min) (point-max))
                        ,after)))))

(provide 'test-helper)

;;; test-helper.el ends here
