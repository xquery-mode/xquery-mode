;;; xquery-mode-test.el --- xquery-mode test suite

;;; Commentary:

;;; Code:

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
          (xquery-mode)
          (indent-region (point-min) (point-max))
          (should (string= (buffer-substring-no-properties (point-min) (point-max))
                           ,fixture)))))))

(provide 'xquery-mode-test)

;;; xquery-mode-test.el ends here
