
declare private function rest-impl:uri-matches(
  $request as element(rest:request),
  $uri as xs:string,
  $raise-errors as xs:boolean)
as xs:boolean
{
  if ($request/@uri and not(matches($uri, $request/@uri)))
  then
    rest-impl:no-match($raise-errors, $rest-impl:INCORRECTURI, concat($uri, " does not match ", $request/@uri))
  else
    true()
};
