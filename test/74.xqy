declare function local:service-line-level-transmission($file-data as node(),$inbound-file-uri as xs:string) as node()
{ (: some comment :)
  let $enc := $file-data/env:source/enc-xml:encounter
  let $env := $file-data/env:metadata
  let $icns := local:fresh-icns(count($enc/enc-xml:lines/enc-xml:line))
  return <transmission xmlns="https://some.ns" processing-method="level1">
           <plan>{$env/env:plan/text()}</plan>
           {
             if ($enc/enc-xml:done)
             then <frequency>x</frequency>
             else ()
           }
           <ids>
             {
               for $pci in $env/env:ids/env:id/text()
               return <id>{$pci}</id>
             }
           </ids>
           <x12-data />
           <data>{xdmp:node-uri($file-data)}</data>
           <generation>{$enc/enc-xml:generation/text()}</generation>

           <transmissions>
             {
               for $line at $index in $enc/enc-xml:lines/enc-xml:line
               return local:service-line-transmission($file-data,$line,$index,$icns[$index],$inbound-file-uri)
             }
           </transmissions>
         </transmission>
};
