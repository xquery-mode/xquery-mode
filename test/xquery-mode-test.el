;;; xquery-mode-test.el --- xquery-mode test suite

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'f)
(require 'xquery-mode)

(setq-default indent-tabs-mode nil)
(setq xquery-mode-indent-style 'native)

(defun unindent ()
  (while (re-search-forward "^ +" nil t)
    (replace-match "")))

(defun indent-with-tabs ()
  (while (re-search-forward "^ +" nil t)
    (replace-match (make-string (random 10) ?\t))))

(defun indent-with-spaces ()
  (while (re-search-forward "^ +" nil t)
    (replace-match (make-string (random 20) ? ))))

(dolist (xqy (f-glob "*.xqy" (and load-file-name (f-dirname load-file-name))))
  (cl-loop for mutation from 1
           for mutate in '(unindent indent-with-tabs indent-with-spaces)
           do (let ((num (f-base xqy))
                    (fixture (f-read xqy)))
                (eval
                 `(ert-deftest ,(intern (format "test-xquery-mode-indent-%s-%d" num mutation)) ()
                    (with-current-buffer (generate-new-buffer (format "*fixture-%s-%d*" ,num ,mutation))
                      (insert ,fixture)
                      (goto-char (point-min))
                      (,mutate)
                      (xquery-mode)
                      (indent-region (point-min) (point-max))
                      (should (string= (buffer-substring-no-properties (point-min) (point-max))
                                       ,fixture))))))))

(provide 'xquery-mode-test)

;;; xquery-mode-test.el ends here
