xquery version "1.0-ml";

module namespace lwcc = "https://lambdawerk.com/xquery/transform-encounter-xml/1.0";

import module namespace functx = "http://www.functx.com" at "/MarkLogic/functx/functx-1.0-nodoc-2007-01.xqy";

declare namespace meta = "http://lambdawerk.com/schema/file-meta-data/1.0";
declare namespace legacy = "http://mcna.net/Schema/claims/1.0";

declare function lwcc:extract-file-metadata($the-doc as node(), $params as node()) as map:map
{
  let $key := $params/context/file-id/text()
  let $sender := $params/context/sender/text()
  let $receiver := $params/context/receiver/text()
  let $uri := "/files/" || $sender || "/" || $receiver || "/" || $key || "-meta.xml"
  let $current-date-time := current-dateTime()
  let $generated := xs:dateTime($params/context/generated-at/text())
  let $received := xs:dateTime($params/context/received-at/text())
  let $archive-url := $params/context/archive-url/text()

  return map:new((
                  map:entry("uri", $uri),
                  map:entry("value",
                            <file xmlns="http://lambdawerk.com/schema/file-meta-data/1.0">
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
    map:new((map:entry("uri", $file-path || "/" || $encounter-id || ".xml"),
             map:entry("value", $encounter)))
};

declare function lwcc:transform($content as map:map, $context as map:map) as map:map*
{
  let $the-doc := map:get($content, "value")
  let $params := xdmp:unquote(map:get($context, "transform_param"))
  let $file-id := $params/context/file-id/text()
  let $file-path := "/files/" || $file-id
  let $file-data := map:new((
                             map:entry("uri", $file-path || ".xml"),
                             map:entry("value",
                                       <encounters-file xmlns="http://lambdawerk.com/schema/encounter-xml/envelope">
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
