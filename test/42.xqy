
declare function rest-impl:rewrite(
  $requests as element(rest:request)*,
  $reqenv as map:map
) as xs:string?
{
  let $uriparams
    := for $name in $request/rest:uri-param/@name
       for $value in map:get($params, $name)
       return
         concat($name, "=", $value)

  let $reqparams
    := for $param in tokenize(if (contains($uri, "?")) then substring-after($uri, "?") else "", "&amp;")
       where not($param = $uriparams)
       return
         $param

  let $addparams
    := if (empty($defparams))
       then ()
       else
         for $name in map:keys($defparams)
         for $value in map:get($defparams, $name)
         return
           concat($name, "=", $value)

  let $eparam   := string-join(($reqparams, $uriparams, $addparams), "&amp;")
  let $sep      := if (contains($endpoint,"?")) then "&amp;" else "?"
  let $result
    := if (empty($endpoint))
       then
         ()
       else
         concat($endpoint,
                if ($eparam = "") then "" else concat($sep, $eparam))
  let $trace := rest-impl:log(concat($uri, " => ", $result))
  return
    $result
};
