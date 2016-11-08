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
            (cl-case (random 4)
              (0 (replace-match ""))
              (1 (replace-match (make-string (random 10) ?\t)))
              (2 (replace-match (make-string (random 20) ? )))
              (3 (replace-match (concat
                                 (cl-loop for i to (random 20)
                                          collect (if (zerop (random 2)) ? ?\t)))))))
          (xquery-mode)
          (xquery-mode-indent-region (point-min) (point-max))
          (should (string= (buffer-substring-no-properties (point-min) (point-max))
                           ,fixture)))))))

(provide 'xquery-mode-test)

;;; xquery-mode-test.el ends here
