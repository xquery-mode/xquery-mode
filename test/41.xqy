
declare function rest-impl:dump-reqenv(
  $reqenv as map:map
) as empty-sequence()
{
  let $uri    := map:get($reqenv, "uri")
  let $method := map:get($reqenv, "method")
  let $accept := map:get($reqenv, "accept")
  let $params := map:get($reqenv, "params")
  return
    (rest-impl:log("Request environment:"),
     rest-impl:log(concat($method, " ", $uri)),
     rest-impl:log(concat("ACCEPT ", $accept)),
     rest-impl:log("PARAMS:"),
     for $name in map:keys($params)
     return
       rest-impl:log(concat("  ", $name, ": (", string-join(map:get($params, $name), ", "), ")")),
     rest-impl:log(""))
};
