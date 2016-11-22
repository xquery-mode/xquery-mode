;;; xquery-mode-test.el --- xquery-mode test suite

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'f)
(require 'xquery-mode)

(setq-default indent-tabs-mode nil)
(setq xquery-mode-indent-style 'native)

(dolist (xqy (f-glob "*.xqy" (and load-file-name (f-dirname load-file-name))))
  (let ((num (f-base xqy))
        (fixture (f-read xqy)))
    (eval
     `(ert-deftest ,(intern (format "test-xquery-mode-indent-%s" num)) ()
        (with-current-buffer (generate-new-buffer (format "*fixture-%s*" ,num))
          (insert ,fixture)
          (goto-char (point-min))
          (while (re-search-forward "^ *" nil t)
            ;; FIXME: Revert this temporarily disabled TAB mutation.
            (replace-match (make-string (random 20) ? )))
          (xquery-mode)
          (indent-region (point-min) (point-max))
          (delete-trailing-whitespace)  ;; FIXME: Remove duty cleanup.
          (should (string= (buffer-substring-no-properties (point-min) (point-max))
                           ,fixture)))))))

(provide 'xquery-mode-test)

;;; xquery-mode-test.el ends here
