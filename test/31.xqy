declare function lwcc:extract-bu-documents-277($tx as node(),$tx-path as xs:string) as map:map*
{
  for $bu at $idx in $tx/*/LOOP2200D
  let $subscriber-patient := $bu/..
  let $tx := $subscriber-patient/..
  let $billing-hl := $tx/LOOP2000C[HL/HL01/text() = $subscriber-patient/HL/HL02/text() ]
  let $loop2000B := $tx/LOOP2000B[HL/HL01/text() = $billing-hl/HL/HL02/text()]
  let $loop2000A := $tx/LOOP2000A[HL/HL01/text() = $loop2000B/HL/HL02/text()]
  let $unit := <business-unit>
                 { $loop2000A }
                 { $loop2000B }
                 { $billing-hl }
                 {
                   element { name($subscriber-patient) }
                           {
                             $subscriber-patient/@*,
                             $subscriber-patient/LOOP2100D,
                             $bu
                           }
                 }
               </business-unit>

  return map:new((
                  map:entry("uri",$tx-path || "/" || $idx || ".xml"),
                  map:entry("value",document {functx:change-element-ns-deep($unit,
                                                                            "http://lambdawerk.com/schema/x12n-xml/277ca",
                                                                            "ca")})))
};
