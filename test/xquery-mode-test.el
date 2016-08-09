;;; xquery-mode-test.el --- xquery-mode test suite

;;; Commentary:

;;; Code:

(define-indent-test function-body-first-line ()
  "Indent first line of function body."
  "
declare function local:get-provider-group-membership($mpf-provider as element(provider))
{
				(for $group-membership in $mpf-provider/provider-group-membership
" "
declare function local:get-provider-group-membership($mpf-provider as element(provider))
{
  (for $group-membership in $mpf-provider/provider-group-membership
")

(define-indent-test flwor-expression-in-brackets ()
  "Indent FLWOR excression within square brackets."
  "
(for $group-membership in $mpf-provider/provider-group-membership
				order by $group-membership/group-membership-effective-date descending
" "
(for $group-membership in $mpf-provider/provider-group-membership
 order by $group-membership/group-membership-effective-date descending
 ")

(define-indent-test flwor-expression-keyword-on-next-line ()
  "Indent line started with RETURN keyword to same column as previous line started with ORDER BY."
  "
								order by $group-membership/group-membership-effective-date descending
								return $group-membership)[1]
" "
order by $group-membership/group-membership-effective-date descending
return $group-membership)[1]
")

(define-indent-test inner-xml-tag ()
  "Inner XML tags should indent with nesting."
  "
<html>
<head>
<title>Access points with an Organization TPI</title>
" "
<html>
  <head>
    <title>Access points with an Organization TPI</title>
")

(provide 'xquery-mode-test)

;;; xquery-mode-test.el ends here
