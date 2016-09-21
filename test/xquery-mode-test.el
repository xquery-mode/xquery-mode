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

(define-indent-test "
xquery version \"1.0-ml\";

declare namespace c = \"http://mcna.net/Schema/claims/1.0\";

declare variable $claim-id as xs:string external := \"10123935\";

declare variable $claim := doc(concat('/claims/', $claim-id, '.xml'))/c:claim;

declare function local:validate-with-tpi($tpi as xs:string, $claim as element()) as xs:string* {

let $access-point := $claim/c:access-point
let $facility := $access-point/c:facility
let $facility-type := $facility/c:type/text()
let $provider := $access-point/c:provider

let $mpf-entry := doc(concat('/tx-mpf/', substring($tpi, 1, 7), '.xml'))/group/provider[tpi = $tpi]
let $mpf-group-membership := $mpf-entry/provider-group-membership[last()]
let $group-tpi := $mpf-group-membership/group-tpisuffix-code
let $mpf-group-entry := if ($group-tpi) then
doc(concat('/tx-mpf/', substring($group-tpi, 1, 7), '.xml'))/group/provider[tpi = $group-tpi]
else
()
return
if ($facility-type = \"GROUP\") then
(
(: GROUP checks :)
if ($mpf-group-membership) then
()
else
\"claim indicates GROUP facility but TPI does not point to provider with provider-group-membership record\",

if ($mpf-entry/npi-api/npi-api != $provider/c:npi) then
\"rendering provider NPI does not match MPF npi-api record\"
else
(),

if ($mpf-group-membership/group-npi-api != $facility/c:npi) then
\"billing provider NPI in claim does not match NPI in MPF group membership record\"
else
(),

if ($mpf-group-entry/provider-tax-id/irs-number != $facility/c:ein) then
\"billing provider tax ID does not match EIN in MPF\"
else
()
)
else
(
(: INDIVIDUAL checks :)
if ($mpf-group-membership) then
\"claim indicates INDIVIDUAL facility but TPI points to provider with provider-group-membership record\"
else
(),

if ($mpf-entry/npi-api/npi-api != $provider/c:npi) then
\"rendering provider NPI does not match MPF npi-api record\"
else
())
};

<claim-validations>
{
for $claim in subsequence(/c:claim, 1, 500)
let $tpi := $claim/c:access-point/c:medicaid-id/text()
let $errors := if ($tpi) then
local:validate-with-tpi($tpi, $claim)
else
()
return
if ($errors) then
<cv:claim id=\"{$claim/@c:id}\">
{ for $message in $errors return <cv:error>{ $message }</cv:error> }
</cv:claim>
else
()
}
</claim-validations>
" "
xquery version \"1.0-ml\";

declare namespace c = \"http://mcna.net/Schema/claims/1.0\";

declare variable $claim-id as xs:string external := \"10123935\";

declare variable $claim := doc(concat('/claims/', $claim-id, '.xml'))/c:claim;

declare function local:validate-with-tpi($tpi as xs:string, $claim as element()) as xs:string* {

  let $access-point := $claim/c:access-point
  let $facility := $access-point/c:facility
  let $facility-type := $facility/c:type/text()
  let $provider := $access-point/c:provider

  let $mpf-entry := doc(concat('/tx-mpf/', substring($tpi, 1, 7), '.xml'))/group/provider[tpi = $tpi]
  let $mpf-group-membership := $mpf-entry/provider-group-membership[last()]
  let $group-tpi := $mpf-group-membership/group-tpisuffix-code
  let $mpf-group-entry := if ($group-tpi) then
                            doc(concat('/tx-mpf/', substring($group-tpi, 1, 7), '.xml'))/group/provider[tpi = $group-tpi]
                          else
                            ()
  return
    if ($facility-type = \"GROUP\") then
      (
       (: GROUP checks :)
       if ($mpf-group-membership) then
         ()
       else
         \"claim indicates GROUP facility but TPI does not point to provider with provider-group-membership record\",

       if ($mpf-entry/npi-api/npi-api != $provider/c:npi) then
         \"rendering provider NPI does not match MPF npi-api record\"
       else
         (),

       if ($mpf-group-membership/group-npi-api != $facility/c:npi) then
         \"billing provider NPI in claim does not match NPI in MPF group membership record\"
       else
         (),

       if ($mpf-group-entry/provider-tax-id/irs-number != $facility/c:ein) then
         \"billing provider tax ID does not match EIN in MPF\"
       else
         ()
      )
    else
      (
       (: INDIVIDUAL checks :)
       if ($mpf-group-membership) then
         \"claim indicates INDIVIDUAL facility but TPI points to provider with provider-group-membership record\"
       else
         (),

       if ($mpf-entry/npi-api/npi-api != $provider/c:npi) then
         \"rendering provider NPI does not match MPF npi-api record\"
       else
         ())
};

<claim-validations>
  {
    for $claim in subsequence(/c:claim, 1, 500)
    let $tpi := $claim/c:access-point/c:medicaid-id/text()
    let $errors := if ($tpi) then
                     local:validate-with-tpi($tpi, $claim)
                   else
                     ()
    return
      if ($errors) then
        <cv:claim id=\"{$claim/@c:id}\">
          { for $message in $errors return <cv:error>{ $message }</cv:error> }
        </cv:claim>
      else
        ()
  }
</claim-validations>
")

(define-indent-test "
declare function local:validate-with-tpi($tpi as xs:string, $claim as element()) as xs:string* {

let $access-point := $claim/c:access-point
let $facility := $access-point/c:facility
" "
declare function local:validate-with-tpi($tpi as xs:string, $claim as element()) as xs:string* {

  let $access-point := $claim/c:access-point
  let $facility := $access-point/c:facility
")

(define-indent-test "
let $mpf-group-entry := if ($group-tpi) then
  doc(concat('/tx-mpf/', substring($group-tpi, 1, 7), '.xml'))/group/provider[tpi = $group-tpi]
else
  ()
" "
let $mpf-group-entry := if ($group-tpi) then
                          doc(concat('/tx-mpf/', substring($group-tpi, 1, 7), '.xml'))/group/provider[tpi = $group-tpi]
                        else
                          ()
")

(define-indent-test "
let $mpf-group-entry := if ($group-tpi) then
                          doc(concat('/tx-mpf/', substring($group-tpi, 1, 7), '.xml'))/group/provider[tpi = $group-tpi]
                        else
                          ()
                          return
" "
let $mpf-group-entry := if ($group-tpi) then
                          doc(concat('/tx-mpf/', substring($group-tpi, 1, 7), '.xml'))/group/provider[tpi = $group-tpi]
                        else
                          ()
return
")

(define-indent-test "
(
if ($mpf-group-membership) then
()
else
\"claim indicates GROUP facility but TPI does not point to provider with provider-group-membership record\",
" "
(
 if ($mpf-group-membership) then
   ()
 else
   \"claim indicates GROUP facility but TPI does not point to provider with provider-group-membership record\",
")

(define-indent-test "
    if ($facility-type = \"GROUP\") then
      (
       if ($mpf-group-entry/provider-tax-id/irs-number != $facility/c:ein) then
         \"billing provider tax ID does not match EIN in MPF\"
       else
         ()
)
" "
if ($facility-type = \"GROUP\") then
  (
   if ($mpf-group-entry/provider-tax-id/irs-number != $facility/c:ein) then
     \"billing provider tax ID does not match EIN in MPF\"
   else
     ()
  )
")

(define-indent-test "
if ($facility-type = \"GROUP\") then
(
if ($mpf-group-entry/provider-tax-id/irs-number != $facility/c:ein) then
\"billing provider tax ID does not match EIN in MPF\"
else
()
)
else
(
if ($mpf-entry/npi-api/npi-api != $provider/c:npi) then
\"rendering provider NPI does not match MPF npi-api record\"
else
())
" "
if ($facility-type = \"GROUP\") then
  (
   if ($mpf-group-entry/provider-tax-id/irs-number != $facility/c:ein) then
     \"billing provider tax ID does not match EIN in MPF\"
   else
     ()
  )
else
  (
   if ($mpf-entry/npi-api/npi-api != $provider/c:npi) then
     \"rendering provider NPI does not match MPF npi-api record\"
   else
     ())
")

(define-indent-test "
<claim-validations>
{
for $claim in subsequence(/c:claim, 1, 500)
let $tpi := $claim/c:access-point/c:medicaid-id/text()
let $errors := if ($tpi) then
local:validate-with-tpi($tpi, $claim)
else
()
return
if ($errors) then
<cv:claim id=\"{$claim/@c:id}\">
{ for $message in $errors return <cv:error>{ $message }</cv:error> }
</cv:claim>
else
()
}
</claim-validations>
" "
<claim-validations>
  {
    for $claim in subsequence(/c:claim, 1, 500)
    let $tpi := $claim/c:access-point/c:medicaid-id/text()
    let $errors := if ($tpi) then
                     local:validate-with-tpi($tpi, $claim)
                   else
                     ()
    return
      if ($errors) then
        <cv:claim id=\"{$claim/@c:id}\">
          { for $message in $errors return <cv:error>{ $message }</cv:error> }
        </cv:claim>
      else
        ()
  }
</claim-validations>
")

(define-indent-test "
xquery version \"1.0-ml\";

module namespace lwcc = \"https://lambdawerk.com/xquery/transform-encounter-xml/1.0\";

import module namespace functx = \"http://www.functx.com\" at \"/MarkLogic/functx/functx-1.0-nodoc-2007-01.xqy\";

declare namespace meta = \"http://lambdawerk.com/schema/file-meta-data/1.0\";
declare namespace legacy = \"http://mcna.net/Schema/claims/1.0\";

declare function lwcc:extract-file-metadata($the-doc as node(), $params as node()) as map:map
{
let $key := $params/context/file-id/text()
let $sender := $params/context/sender/text()
let $receiver := $params/context/receiver/text()
let $uri := \"/files/\" || $sender || \"/\" || $receiver || \"/\" || $key || \"-meta.xml\"
let $current-date-time := current-dateTime()
let $generated := xs:dateTime($params/context/generated-at/text())
let $received := xs:dateTime($params/context/received-at/text())
let $archive-url := $params/context/archive-url/text()

return map:new((
map:entry(\"uri\", $uri),
map:entry(\"value\",
<file xmlns=\"http://lambdawerk.com/schema/file-meta-data/1.0\">
<key> { $key } </key>
<name>{ $params/context/filename/text() }</name>
<sender>{ $sender }</sender>
<receiver>{ $receiver }</receiver>
<loaded-at>{ $current-date-time }</loaded-at>
<format>encounters</format>
<variant>{ $receiver }</variant>
<generated-at>{ $generated }</generated-at>
<received-at>{ $received }</received-at>
{
if ($archive-url) then
<archive-url>{ $archive-url }</archive-url>
else
()
}
</file>)))
};

declare function lwcc:extract-encounter-documents($encounters as node()*, $file-path as xs:string) as map:map*
{
for $encounter in $encounters
let $encounter-id := $encounter/@id
return
map:new((map:entry(\"uri\", $file-path || \"/\" || $encounter-id || \".xml\"),
map:entry(\"value\", $encounter)))
};

declare function lwcc:transform($content as map:map, $context as map:map) as map:map*
{
let $the-doc := map:get($content, \"value\")
let $params := xdmp:unquote(map:get($context, \"transform_param\"))
let $file-id := $params/context/file-id/text()
let $file-path := \"/files/\" || $file-id
let $file-data := map:new((
map:entry(\"uri\", $file-path || \".xml\"),
map:entry(\"value\",
<encounters-file xmlns=\"http://lambdawerk.com/schema/encounter-xml/envelope\">
<file-id>{ $the-doc/legacy:claims/@file-id/text() }</file-id>
{
if ($the-doc/legacy:description) then
<description>{ $the-doc/legacy:description }</description>
else
()
}
</encounters-file>)))
return ($file-data,
lwcc:extract-file-metadata($the-doc, $params),
lwcc:extract-encounter-documents($the-doc/legacy:claims/legacy:claim, $file-path))
};
" "
xquery version \"1.0-ml\";

module namespace lwcc = \"https://lambdawerk.com/xquery/transform-encounter-xml/1.0\";

import module namespace functx = \"http://www.functx.com\" at \"/MarkLogic/functx/functx-1.0-nodoc-2007-01.xqy\";

declare namespace meta = \"http://lambdawerk.com/schema/file-meta-data/1.0\";
declare namespace legacy = \"http://mcna.net/Schema/claims/1.0\";

declare function lwcc:extract-file-metadata($the-doc as node(), $params as node()) as map:map
{
  let $key := $params/context/file-id/text()
  let $sender := $params/context/sender/text()
  let $receiver := $params/context/receiver/text()
  let $uri := \"/files/\" || $sender || \"/\" || $receiver || \"/\" || $key || \"-meta.xml\"
  let $current-date-time := current-dateTime()
  let $generated := xs:dateTime($params/context/generated-at/text())
  let $received := xs:dateTime($params/context/received-at/text())
  let $archive-url := $params/context/archive-url/text()

  return map:new((
                  map:entry(\"uri\", $uri),
                  map:entry(\"value\",
                            <file xmlns=\"http://lambdawerk.com/schema/file-meta-data/1.0\">
                              <key> { $key } </key>
                              <name>{ $params/context/filename/text() }</name>
                              <sender>{ $sender }</sender>
                              <receiver>{ $receiver }</receiver>
                              <loaded-at>{ $current-date-time }</loaded-at>
                              <format>encounters</format>
                              <variant>{ $receiver }</variant>
                              <generated-at>{ $generated }</generated-at>
                              <received-at>{ $received }</received-at>
                              {
                                if ($archive-url) then
                                  <archive-url>{ $archive-url }</archive-url>
                                else
                                  ()
                              }
                            </file>)))
};

declare function lwcc:extract-encounter-documents($encounters as node()*, $file-path as xs:string) as map:map*
{
  for $encounter in $encounters
  let $encounter-id := $encounter/@id
  return
    map:new((map:entry(\"uri\", $file-path || \"/\" || $encounter-id || \".xml\"),
             map:entry(\"value\", $encounter)))
};

declare function lwcc:transform($content as map:map, $context as map:map) as map:map*
{
  let $the-doc := map:get($content, \"value\")
  let $params := xdmp:unquote(map:get($context, \"transform_param\"))
  let $file-id := $params/context/file-id/text()
  let $file-path := \"/files/\" || $file-id
  let $file-data := map:new((
                             map:entry(\"uri\", $file-path || \".xml\"),
                             map:entry(\"value\",
                                       <encounters-file xmlns=\"http://lambdawerk.com/schema/encounter-xml/envelope\">
                                         <file-id>{ $the-doc/legacy:claims/@file-id/text() }</file-id>
                                         {
                                           if ($the-doc/legacy:description) then
                                             <description>{ $the-doc/legacy:description }</description>
                                           else
                                             ()
                                         }
                                       </encounters-file>)))
  return ($file-data,
          lwcc:extract-file-metadata($the-doc, $params),
          lwcc:extract-encounter-documents($the-doc/legacy:claims/legacy:claim, $file-path))
};
")

(define-indent-test "
return map:new((
map:entry(\"uri\", $uri),
map:entry(\"value\",
" "
return map:new((
                map:entry(\"uri\", $uri),
                map:entry(\"value\",
")

(define-indent-test "
<loaded-at>{ $current-date-time }</loaded-at>
<format>encounters</format>
" "
<loaded-at>{ $current-date-time }</loaded-at>
<format>encounters</format>
")

(define-indent-test "
{
if ($the-doc/legacy:description) then
<description>{ $the-doc/legacy:description }</description>
else
()
}
" "
{
  if ($the-doc/legacy:description) then
    <description>{ $the-doc/legacy:description }</description>
  else
    ()
}
")

(define-indent-test "
  let $file-data := map:new((
  map:entry(\"uri\", $file-path || \".xml\"),
" "
let $file-data := map:new((
                           map:entry(\"uri\", $file-path || \".xml\"),
")

(define-indent-test "
declare function lwcc:extract-bu-documents-277($tx as node(),$tx-path as xs:string) as map:map*
{
for $bu at $idx in $tx/*/LOOP2200D
let $subscriber-patient := $bu/..
let $tx := $subscriber-patient/..
let $billing-hl := $tx/LOOP2000C[HL/HL01/text() = $subscriber-patient/HL/HL02/text() ]
let $loop2000B := $tx/LOOP2000B[HL/HL01/text() = $billing-hl/HL/HL02/text()]
let $loop2000A := $tx/LOOP2000A[HL/HL01/text() = $loop2000B/HL/HL02/text()]
let $unit := <business-unit>
{ $loop2000A }
{ $loop2000B }
{ $billing-hl }
{
element { name($subscriber-patient) }
{
$subscriber-patient/@*,
$subscriber-patient/LOOP2100D,
$bu
}
}
</business-unit>

return map:new((
map:entry(\"uri\",$tx-path || \"/\" || $idx || \".xml\"),
map:entry(\"value\",document {functx:change-element-ns-deep($unit,
\"http://lambdawerk.com/schema/x12n-xml/277ca\",
\"ca\")})))
};
" "
declare function lwcc:extract-bu-documents-277($tx as node(),$tx-path as xs:string) as map:map*
{
  for $bu at $idx in $tx/*/LOOP2200D
  let $subscriber-patient := $bu/..
  let $tx := $subscriber-patient/..
  let $billing-hl := $tx/LOOP2000C[HL/HL01/text() = $subscriber-patient/HL/HL02/text() ]
  let $loop2000B := $tx/LOOP2000B[HL/HL01/text() = $billing-hl/HL/HL02/text()]
  let $loop2000A := $tx/LOOP2000A[HL/HL01/text() = $loop2000B/HL/HL02/text()]
  let $unit := <business-unit>
                 { $loop2000A }
                 { $loop2000B }
                 { $billing-hl }
                 {
                   element { name($subscriber-patient) }
                   {
                     $subscriber-patient/@*,
                     $subscriber-patient/LOOP2100D,
                     $bu
                   }
                 }
               </business-unit>

  return map:new((
                  map:entry(\"uri\",$tx-path || \"/\" || $idx || \".xml\"),
                  map:entry(\"value\",document {functx:change-element-ns-deep($unit,
                                                                            \"http://lambdawerk.com/schema/x12n-xml/277ca\",
                                                                            \"ca\")})))
};
")

(define-indent-test "
declare function lwcc:transform($content as map:map, $context as map:map) as map:map*
{
let $the-doc := map:get($content,\"value\")/x12-file
let $params := xdmp:unquote(map:get($context,\"transform_param\"))
let $file-id := $params/context/file-id/text()
let $file-path := \"/files/\" || $file-id
let $file-data := map:new((
map:entry(\"uri\", $file-path || \".xml\"),
map:entry(\"value\", functx:change-element-ns-deep(element {\"x12-file\"} {$the-doc/x12-filename,
$the-doc/key,
$the-doc/ISA},
\"http://lambdawerk.com/schema/x12n-xml/envelope\",
\"env\"))))
return ($file-data,
lwcc:extract-file-metadata($the-doc,$params),
lwcc:extract-group-documents ($the-doc,$file-path))
};" "
declare function lwcc:transform($content as map:map, $context as map:map) as map:map*
{
  let $the-doc := map:get($content,\"value\")/x12-file
  let $params := xdmp:unquote(map:get($context,\"transform_param\"))
  let $file-id := $params/context/file-id/text()
  let $file-path := \"/files/\" || $file-id
  let $file-data := map:new((
                             map:entry(\"uri\", $file-path || \".xml\"),
                             map:entry(\"value\", functx:change-element-ns-deep(element {\"x12-file\"} {$the-doc/x12-filename,
                                                                                                    $the-doc/key,
                                                                                                    $the-doc/ISA},
                                                                              \"http://lambdawerk.com/schema/x12n-xml/envelope\",
                                                                              \"env\"))))
  return ($file-data,
          lwcc:extract-file-metadata($the-doc,$params),
          lwcc:extract-group-documents ($the-doc,$file-path))
};")

(provide 'xquery-mode-test)

;;; xquery-mode-test.el ends here
