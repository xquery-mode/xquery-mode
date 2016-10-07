declare function lwcc:transform($content as map:map, $context as map:map) as map:map*
{
  let $the-doc := map:get($content,"value")/x12-file
  let $params := xdmp:unquote(map:get($context,"transform_param"))
  let $file-id := $params/context/file-id/text()
  let $file-path := "/files/" || $file-id
  let $file-data := map:new((
                             map:entry("uri", $file-path || ".xml"),
                             map:entry("value", functx:change-element-ns-deep(element {"x12-file"} {$the-doc/x12-filename,
                                                                                                    $the-doc/key,
                                                                                                    $the-doc/ISA},
                                                                              "http://lambdawerk.com/schema/x12n-xml/envelope",
                                                                              "env"))))
  return ($file-data,
          lwcc:extract-file-metadata($the-doc,$params),
          lwcc:extract-group-documents ($the-doc,$file-path))
};
