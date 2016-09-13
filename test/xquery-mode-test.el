;;; xquery-mode-test.el --- xquery-mode test suite

;;; Commentary:

;;; Code:

(define-indent-test "
declare function local:get-provider-group-membership($mpf-provider as element(provider))
{
				(for $group-membership in $mpf-provider/provider-group-membership
" "
declare function local:get-provider-group-membership($mpf-provider as element(provider))
{
  (for $group-membership in $mpf-provider/provider-group-membership
")

(define-indent-test "
(for $group-membership in $mpf-provider/provider-group-membership
				order by $group-membership/group-membership-effective-date descending
" "
(for $group-membership in $mpf-provider/provider-group-membership
 order by $group-membership/group-membership-effective-date descending
")

(define-indent-test "
								order by $group-membership/group-membership-effective-date descending
								return $group-membership)[1]
" "
order by $group-membership/group-membership-effective-date descending
return $group-membership)[1]
")

(define-indent-test "
<html>
<head>
<title>Access points with an Organization TPI</title>
<meta>Hello</meta>
" "
<html>
  <head>
    <title>Access points with an Organization TPI</title>
    <meta>Hello</meta>
")

(define-indent-test "
<title>Access points with an Organization TPI</title>
<style type=\"text/css\">
" "
<title>Access points with an Organization TPI</title>
<style type=\"text/css\">
")

(define-indent-test "
<foo>
baz
</foo>" "
<foo>
  baz
</foo>")

(define-indent-test "
<tbody>
{
	for $tpi in fn:subsequence()
" "
<tbody>
  {
    for $tpi in fn:subsequence()
")

(define-indent-test "
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

(define-indent-test "
<html>
		<body>
  }" "
<html>
  <body>
}")

(define-indent-test "
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

(define-indent-test "
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

(define-indent-test "
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

(define-indent-test "
return
let $mpf-provider := doc()/provider
let $group-tpi := local:get-provider-group-membership()
" "
return
  let $mpf-provider := doc()/provider
  let $group-tpi := local:get-provider-group-membership()
")

(define-indent-test "
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

(define-indent-test "
if ($facility-type = \"GROUP\") then
$test/xx()
else
$test2/yy()
" "
if ($facility-type = \"GROUP\") then
  $test/xx()
else
  $test2/yy()
")

(define-indent-test "
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

(define-indent-test "xquery version \"1.0-ml\";
(: dump.xqy :)
declare namespace bk = \"http://www.marklogic.com/ns/gs-books\";

<html xmlns=\"http://www.w3.org/1999/xhtml\">
<head>
<title>Database dump</title>
</head>
<body>
<b>XML Content</b>
{
for $book in doc(\"books.xml\")/bk:books/bk:book
return
<pre>
Title: { $book/bk:title/text() }
Author: { ($book/bk:author/bk:first/text(), \" \",
$book/bk:author/bk:last/text()) }
Publisher: { $book/bk:publisher/text() }
</pre>
}
</body>
</html>" "xquery version \"1.0-ml\";
(: dump.xqy :)
declare namespace bk = \"http://www.marklogic.com/ns/gs-books\";

<html xmlns=\"http://www.w3.org/1999/xhtml\">
  <head>
    <title>Database dump</title>
  </head>
  <body>
    <b>XML Content</b>
    {
      for $book in doc(\"books.xml\")/bk:books/bk:book
      return
        <pre>
          Title: { $book/bk:title/text() }
          Author: { ($book/bk:author/bk:first/text(), \" \",
                     $book/bk:author/bk:last/text()) }
          Publisher: { $book/bk:publisher/text() }
        </pre>
    }
  </body>
</html>")

(provide 'xquery-mode-test)

;;; xquery-mode-test.el ends here
