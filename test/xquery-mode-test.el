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
          ;; FIXME: Revert this temporarily disabled mutation.
          (while (re-search-forward "^ +" nil t)
            (replace-match ""))
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

(ert-deftest test-xquery-mode-indent-line-part ()
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

(ert-deftest test-xquery-mode-indent-cdata ()
  "Don't touch CDATA content in any way."
  (with-current-buffer (generate-new-buffer "*fixture-cdata*")
    (insert "<a><![CDATA[Hello  World

  a     b
        a d]]></a>
")
    (goto-char (point-min))
    (xquery-mode)
    (indent-region (point-min) (point-max))
    (should (string= "<a><![CDATA[Hello  World

  a     b
        a d]]></a>
" (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest test-xquery-mode-comment-or-uncomment-region ()
  "Comment and uncomment region should work predictable."
  (with-current-buffer (generate-new-buffer "*fixture-comment-or-uncomment-region*")
    (insert "$fn(.)")
    (xquery-mode)
    (comment-or-uncomment-region (point-min) (point-max))
    (should (string= "(: $fn(.) :)" (buffer-substring-no-properties (point-min) (point-max))))
    (comment-or-uncomment-region (point-min) (point-max))
    (should (string= "$fn(.)" (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest test-xquery-mode-comment-or-uncomment-region-multi-line ()
  "Comment and uncomment region should work predictable."
  (with-current-buffer (generate-new-buffer "*fixture-comment-or-uncomment-region*")
    (insert "
declare function northwind:extract-array(
  $path-to-property as item()*,
  $fn as function(*)
) as json:array?
{
  if (empty($path-to-property))
  then ()
  else json:to-array($path-to-property ! $fn(.))
};
")
    (xquery-mode)
    (comment-or-uncomment-region (point-min) (point-max))
    (should (string= "
(: \n : declare function northwind:extract-array(
 :   $path-to-property as item()*,
 :   $fn as function(*)
 : ) as json:array?
 : {
 :   if (empty($path-to-property))
 :   then ()
 :   else json:to-array($path-to-property ! $fn(.))
 : };
 :)
" (buffer-substring-no-properties (point-min) (point-max))))
    (comment-or-uncomment-region (point-min) (point-max))
    (should (string= "
declare function northwind:extract-array(
  $path-to-property as item()*,
  $fn as function(*)
) as json:array?
{
  if (empty($path-to-property))
  then ()
  else json:to-array($path-to-property ! $fn(.))
};
" (buffer-substring-no-properties (point-min) (point-max))))))

(provide 'xquery-mode-test)

;;; xquery-mode-test.el ends here
