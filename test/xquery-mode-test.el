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

(define-indent-test sequential-xml-tag ()
  "Sequential XML tags must have same indentation column."
  "
<title>Access points with an Organization TPI</title>
<style type=\"text/css\">
" "
<title>Access points with an Organization TPI</title>
<style type=\"text/css\">
")

(define-indent-test xml-tag-value ()
  "Indent XML tag value differ then opening tag."
  "
<foo>
baz
</foo>" "
<foo>
  baz
</foo>")

(define-indent-test flwor-expression-open-bracket ()
  "Indent FLWOR open curly bracket one level dipper."
  "
<tbody>
{
	for $tpi in fn:subsequence()
" "
<tbody>
  {
    for $tpi in fn:subsequence()
")

(define-indent-test flwor-expression-close-bracket ()
  "Indent FLWOR close curly bracket same level as corresponding
open bracket."
  "
<tbody>
{
	for $tpi in fn:subsequence()
	}
" "
<tbody>
  {
    for $tpi in fn:subsequence()
  }
")

(define-indent-test flwor-missed-open-curly-bracket ()
  "Indent close curly bracket to the beginning of line if open
curly bracket is missed."
  "
<html>
		<body>
	}" "
<html>
  <body>
}")

(define-indent-test flwor-expression-nested-curly-brackets ()
  "Indent nested curly brackets with dipper indentation level."
  "
<html><body>
{
	for $act in doc(\"hamlet.xml\")//ACT
	let $speakers := distinct-values($act//SPEAKER)
	return
		<div>
		<h1>{ string($act/TITLE) }</h1>
		<ul>
{
			for $speaker in $speakers
			return <li>{ $speaker }</li>
}
		</ul>
		</div>
}
</body></html>
" "
<html><body>
  {
    for $act in doc(\"hamlet.xml\")//ACT
    let $speakers := distinct-values($act//SPEAKER)
    return
      <div>
        <h1>{ string($act/TITLE) }</h1>
        <ul>
          {
            for $speaker in $speakers
            return <li>{ $speaker }</li>
          }
        </ul>
      </div>
  }
</body></html>
")

(define-indent-test flwor-for-let-let-sequential ()
  "Indent for let lest sequential expression to same column."
  "
{
	for $tpi in fn:subsequence()
	let $provider-id := $tpi/../../provider
	let $facility := $tpi/../../..
" "
{
  for $tpi in fn:subsequence()
  let $provider-id := $tpi/../../provider
  let $facility := $tpi/../../..
")

(define-indent-test flwor-let-order-by-sequential ()
  "Indent sequential let order by statements to the same column."
  "
{
	for $tpi in fn:subsequence()
	let $provider-id := $tpi/../../provider
	order by $provider-id/id
" "
{
  for $tpi in fn:subsequence()
  let $provider-id := $tpi/../../provider
  order by $provider-id/id
")

(define-indent-test flwor-return-let-let-nested ()
  "Indent let statement nested into return statement with dipper column."
  "
return
let $mpf-provider := doc()/provider
let $group-tpi := local:get-provider-group-membership()
" "
return
  let $mpf-provider := doc()/provider
  let $group-tpi := local:get-provider-group-membership()
")

(define-indent-test flwor-multiline-if ()
  "Indent multiline if statement."
  "
return
let $mpf-tin := if ($mpf-group)
then
$mpf-group/provider-tax-id[1]/irs-number/text()
else
$mpf-provider/provider-tax-id[1]/irs-number/text()
" "
return
  let $mpf-tin := if ($mpf-group)
                  then
                    $mpf-group/provider-tax-id[1]/irs-number/text()
                  else
                    $mpf-provider/provider-tax-id[1]/irs-number/text()
")

(define-indent-test flwor-let-where-or-return ()
  "Indent sequential let where return statements on same line.
If where statement have multiline condition expression indent it dipper."
   "
let $mpf-tin := if ($mpf-group) then $mpf-group/provider-tax-id[1]/irs-number/text() else $mpf-provider/provider-tax-id[1]/irs-number/text()
where $facility-npi != $mpf-facility-npi
or $facility-tin != $mpf-tin
or $provider-npi != $mpf-provider-npi
or $name-classification != 'I' or not($mpf-provider-name)
return
<tr>
" "
let $mpf-tin := if ($mpf-group) then $mpf-group/provider-tax-id[1]/irs-number/text() else $mpf-provider/provider-tax-id[1]/irs-number/text()
where $facility-npi != $mpf-facility-npi
      or $facility-tin != $mpf-tin
      or $provider-npi != $mpf-provider-npi
      or $name-classification != 'I' or not($mpf-provider-name)
return
  <tr>
")

(provide 'xquery-mode-test)

;;; xquery-mode-test.el ends here
