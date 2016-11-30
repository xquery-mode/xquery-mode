xquery version "1.0-ml";
declare namespace html = "http://www.w3.org/1999/xhtml";
import module namespace sem = "http://marklogic.com/semantics"  at "/MarkLogic/semantics.xqy";

declare function local:encode($s)
{
  replace($s, '"', '""')
};

declare function local:map2csv($headers as xs:string*, $m as map:map)
{
  string-join (
               for $h in $headers return '"' || local:encode(map:get($m, $h)) || '"',
               ","
              )
};

declare function local:list2csv($list as xs:string*)
{
  string-join (
               for $x in $list return '"' || local:encode($x) || '"',
               ","
              )
};

let $headers := ("s", "p", "o")

return

  (

   local:list2csv($headers)

   ,

   for $row in
       sem:sparql('

PREFIX foaf: <http://xmlns.com/foaf/0.1/>

SELECT ?s ?p ?o
FROM <info:govtrack/people>
WHERE
{
?s foaf:name "Daniel Inouye" ; ?p ?o .
}

')
   return local:map2csv($headers, $row)
  )
