xquery version "1.0-ml";

(: Copyright 2002-2011 Mark Logic Corporation.  All Rights Reserved. :)

declare namespace csvscan = "http://marklogic.com/extension/plugin/csvscan";

import module namespace plugin = "http://marklogic.com/extension/plugin" at "/MarkLogic/plugin/plugin.xqy";
import module namespace info="http://marklogic.com/appservices/infostudio" at "/MarkLogic/appservices/infostudio/info.xqy";
import module namespace infodev="http://marklogic.com/appservices/infostudio/dev" at "/MarkLogic/appservices/infostudio/infodev.xqy";

declare namespace ml="http://marklogic.com/appservices/mlogic";
declare namespace lbl="http://marklogic.com/xqutils/labels";

declare namespace zip="xdmp:zip";

declare default function namespace "http://www.w3.org/2005/xpath-functions";

(:~ Map of capabilities implemented by this Plugin.
 :
 : Required capabilities for all Collectors
 : - http://marklogic.com/appservices/infostudio/collector/model
 : - http://marklogic.com/appservices/infostudio/collector/start
 : - http://marklogic.com/appservices/string
 :)

declare function csvscan:capabilities()
as map:map
{
  let $map := map:map()
  let $_ := map:put($map, "http://marklogic.com/appservices/infostudio/collector/model", xdmp:function(xs:QName("csvscan:model")))
  let $_ := map:put($map, "http://marklogic.com/appservices/infostudio/collector/start", xdmp:function(xs:QName("csvscan:start")))
  let $_ := map:put($map, "http://marklogic.com/appservices/infostudio/collector/config-view", xdmp:function(xs:QName("csvscan:view")))
  let $_ := map:put($map, "http://marklogic.com/appservices/infostudio/collector/cancel", xdmp:function(xs:QName("csvscan:cancel")))
  let $_ := map:put($map, "http://marklogic.com/appservices/infostudio/collector/validate", xdmp:function(xs:QName("csvscan:validate")))
  let $_ := map:put($map, "http://marklogic.com/appservices/string", xdmp:function(xs:QName("csvscan:string")))
  return $map
};

(:~ Data model underlying UI; represents the data to be passed into invoke :)
declare function csvscan:model()
as element(plugin:plugin-model)
{
  <plugin:plugin-model>
    <plugin:data>
      <dir>Enter directory here</dir>
      <delimiter>,</delimiter>
      <headers>true</headers>
    </plugin:data>
  </plugin:plugin-model>
};

(:~ Invoke the plugin :)
declare function csvscan:start(
  $model as element(),
  $ticket-id as xs:string,
  $policy-deltas as element(info:options)?
)
as empty-sequence()
{
  let $dir := $model/plugin:data/dir/string()
  let $function := xdmp:function(xs:QName("csvscan:process-file"))
  return infodev:filesystem-walk($dir,$ticket-id,$function,$policy-deltas,$model)
};

declare function csvscan:process-file(
  $document as node()?,
  $source-location as xs:string,
  $ticket-id as xs:string,
  $policy-deltas as element(info:options)?,
  $context as item()?)
as xs:string*
{
  let $document := infodev:get-file($source-location,$ticket-id,$policy-deltas)
  let $mimetype := xdmp:uri-content-type($source-location)

  let $use-headers := if($context/plugin:data/headers/string() eq "true") then
                        fn:true()
                      else
                        fn:false()

  let $delimiter := $context/plugin:data/delimiter/string()

  let $result :=
    if(fn:ends-with($mimetype,"/csv"))
    then
      try {
        let $csv-name := fn:concat(fn:substring-before($source-location,".csv"),".xml")
        let $lines :=  fn:tokenize($document,"[\n\r]+")
        let $line := $lines[1]
        let $header-elements := if($use-headers) then
                                  let $headers := fn:tokenize($line, $delimiter)
                                  let $header-elems := for $h in $headers
                                                       let $upd := fn:replace($h," ","_")
                                                       return if(fn:matches($upd,"^([a-zA-Z]+[_0-9-]*)+[a-zA-Z0-9]+$")) then
                                                                element{fn:QName((),$upd)} {$upd}
                                                              else
                                                                fn:error(xs:QName("ERROR"), "Value Cannot Be Used As Element Name. Please Reconfigure to Use Defaults.")
                                  return $header-elems
                                else
                                  ()

        let $csv:=  <csv>{
          if(fn:empty($header-elements)) then
            for $line in $lines[1 to fn:count($lines)-1]
            return <row>{
              let $l := fn:tokenize($line,$delimiter)
              return for $ln at $idx in $l
                     return element {fn:concat("column",$idx)} {$ln}
            }</row>
          else
            for $l in $lines[2 to fn:count($lines)-1]
            let $line-vals := fn:tokenize($l, $delimiter)
            return <row>{
              for $lv at $d in $line-vals
              return element {fn:name($header-elements[$d])} {$lv}
            }</row>
        }</csv>

        return infodev:ingest($csv, $csv-name,$ticket-id,$policy-deltas)

      } catch($e) {
        (infodev:handle-error($ticket-id, $source-location, $e), xdmp:log(fn:concat("ERROR",$e)))
      }
    else
      let $current-total := if(fn:empty(xs:integer(info:ticket($ticket-id)/info:total-documents))) then
                              0
                            else
                              xs:integer(info:ticket($ticket-id)/info:total-documents)

      let $total-count := $current-total - 1
      let $set-total := infodev:ticket-set-total-documents($ticket-id, $total-count)
      return  ()
  return $result

};
