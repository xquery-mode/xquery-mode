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

(ert-deftest test-xquery-mode-indent-buffer-part ()
  "Indent only part of the buffer."
  (with-current-buffer (generate-new-buffer "*fixture-buffer-part*")
    (insert "
  return
<tr>")
    (goto-char (point-min))
    (xquery-mode)
    (indent-region 11 15)
    (should (string= "
  return
    <tr>" (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest test-xquery-mode-indent-line ()
  "Indent current line."
  (with-current-buffer (generate-new-buffer "*fixture-buffer-part*")
    (insert "
  return
<tr>
<td>")
    (goto-char (point-min))
    (xquery-mode)
    (goto-char 12)
    (indent-for-tab-command)
    (should (string= "
  return
    <tr>
<td>" (buffer-substring-no-properties (point-min) (point-max))))))

(provide 'xquery-mode-test)

;;; xquery-mode-test.el ends here
