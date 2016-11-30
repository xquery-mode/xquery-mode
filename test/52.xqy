(:~
   TODO make descriptive comment for extract-instance-Run
 :)
declare function race:extract-instance-Run(
  $source-node as node()
) as map:map
{
  let $runner-name := string($source-node/runByRunner)
  let $runnerDoc := cts:search( collection("raw"), cts:json-property-value-query("name", $runner-name))
  return
    json:object()
    =>map:with('$type', 'Run')
    =>map:with('$attachments', xdmp:quote($source-node))
    =>map:with('id',                     xs:string($source-node/id))
    =>map:with('date',                   xs:date($source-node/date))
    =>map:with('distance',               xs:decimal($source-node/distance))
    =>map:with('distanceLabel',          xs:string($source-node/distanceLabel))
    =>map:with('duration',               functx:dayTimeDuration((), (), xs:decimal($source-node/duration), ()))
    =>map:with('runByRunner',            race:extract-instance-Runner($runnerDoc))
};
