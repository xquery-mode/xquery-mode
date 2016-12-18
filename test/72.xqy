declare function local:make-inbound-file($meta as node(),$icns as xs:string*) as node()
{
  element {"ief:inbound-test-file"}
          { element {"ief:name"} {$meta/meta:name/text()},
            element {"ief:file-key"} {$meta/meta:file/meta:key/text()},
            element {"ief:meta-uri"} {xdmp:node-uri($meta)},
            element {"ief:load-start-time"} {$meta/meta:file/meta:loaded-at/text()},
            element {"ief:load-end-time"} {$now}, (: assuming fn is called at the end and merging is considered a separate process :)
            element {"ief:icns"} {for $icn in $icns
                                  return element {"ief:icn"}
                                                 {$icn}}}

};

declare function local:test-level-transmission($file-data as node(),$inbound-test-file-uri as xs:string) as node()
{ (: carries both previous-icn /next pointers:)
  let $enc := $file-data/env:source/enc-xml:test
  let $env := $file-data/env:metadata
  return <test-transmission xmlns="https://some.ns" processing-method="test-level">
           <icn>{local:fresh-icns(1)}</icn>
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
           <x12-data />
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
