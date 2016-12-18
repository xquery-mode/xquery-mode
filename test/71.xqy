declare function local:test-level-transmission($file-data as node(),$inbound-test-file-uri as xs:string) as node()
{ (: carries both previous-icn /next pointers:)
  let $enc := $file-data/env:source/enc-xml:test
  let $env := $file-data/env:metadata
  return <test-transmission xmlns="https://some.ns" processing-method="test-level">
           <icn>{local:icns(1)}</icn>
           <remote-icn />
           <plan>{$env/env:plan/text()}</plan>
           <!-- FIXME this would mandate explicit element constructors -->
           <frequency>{if ($enc/enc-xml:voided)
                       then "void"
                       else ()}</frequency>
           <ids>
             {
               for $pci in $env/env:ids/env:id/text()
               return <id>{$pci}</id>
             }
           </ids>
           <data />
           <test-data>{xdmp:node-uri($file-data)}</test-data>
           <generation>{$enc/enc-xml:generation/text()}</generation>
           <status>
             <code>pending</code>
             <timestamp>{$now}</timestamp>
             <cause>{$inbound-test-file-uri}</cause>
           </status>
           <status-history />
         </test-transmission>
};
