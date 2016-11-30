declare function facet:extract(
  $parsed as element(),
  $facet-name as xs:string)
as schema-element(cts:query)*
{
  if ($parsed/self::cts:properties-query
      or $parsed/self::cts:element-query
      or $parsed/self::cts:element-attribute-pair-geospatial-query
      or $parsed/self::cts:element-child-geospatial-query
      or $parsed/self::cts:element-geospatial-query
      or $parsed/self::cts:element-pair-geospatial-query) then
    () (: Oh, forget it, we don't care about these. Property, element, and geo queries should never produce facets. :)
  else
  if ($parsed[@qtextpre eq $facet-name or fn:contains(@qtextconst,$facet-name)]) then
    $parsed
  else
    for $child in $parsed/*
    return facet:extract($child, $facet-name)
};
