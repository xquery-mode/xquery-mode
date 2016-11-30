declare private function sem:inf-owl2rl-scm-sco($m as map:map)
as  empty-sequence()
{
  for  $t_x_y in sem:evT( sem:query-p( $sem:P-RDF-SUBCLASS ) ),
       $x in $t_x_y/s/text(),
       $y in $t_x_y/o/text()
  for  $z in sem:ev1( $sem:QN-O, (sem:query-s( $y ), sem:query-p( $sem:P-RDF-SUBCLASS ) ))
  let $key := sem:uri-for-tuple($x, $sem:P-RDF-SUBCLASS, $z, '')
  where ( ($x != $y) and ($y != $z)  and (map:count($m) lt 10000) and ( not (fn:doc-available($key)) ) )
  return map:put(
                 $m,
                 $key,
                 sem:tuple($x, $sem:P-RDF-SUBCLASS, $z, ''))
};

declare private function sem:inf-owl2rl-scm-spo($m as map:map)
as  empty-sequence()
{
  for $t_x_y in sem:evT( sem:query-p( $sem:P-RDF-SUBPROPERTY ) )
      , $x in $t_x_y/s/text()
      , $y in $t_x_y/o/text()
  let $key := sem:uri-for-tuple($x, $sem:P-RDF-SUBPROPERTY, $z, '')
  where ( ($x != $y) and ($y != $z)  and (map:count($m) lt 10000) and ( not (fn:doc-available($key)) ) )
  return map:put(
                 $m,
                 $key,
                 sem:tuple($x, $sem:P-RDF-SUBPROPERTY, $z, ''))
};

(: semantic.xqy :)
