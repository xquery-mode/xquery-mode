
declare private function rest-impl:apply(
  $request as element(rest:request),
  $reqenv as map:map,
  $conditions as element()*,
  $raise-errors as xs:boolean)
as xs:boolean*
{
  for $cond in $conditions
  let $value
    := typeswitch($cond)
       case element(rest:or)
         return rest-impl:or($request, $reqenv, $cond/*, $raise-errors)
       case element(rest:and)
         return rest-impl:and($request, $reqenv, $cond/*, $raise-errors)
       case element(rest:function)
         return rest-impl:function($request, $reqenv, $cond, $raise-errors)
       case element(rest:auth)
         return rest-impl:auth($request, $reqenv, $cond, $raise-errors)
       case element(rest:user-agent)
         return rest-impl:user-agent($request, $reqenv, $cond, $raise-errors)
       case element(rest:accept)
         return rest-impl:accepts-type($request, $reqenv, $cond, $raise-errors)
       default
         return error($rest-impl:INVALIDCONDITION, concat(node-name($cond), " is not a condition"))
  return
    $value
};
