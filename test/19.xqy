declare function local:validate-with-tpi($tpi as xs:string, $claim as element()) as xs:string* {

  let $access-point := $claim/c:access-point
  let $facility := $access-point/c:facility
