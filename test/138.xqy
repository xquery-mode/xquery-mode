
declare function asc:browse()
as element(div)
{
  <div class="front-page-content">
    {let $front-page := $config:OPTIONS/search:constraint[search:annotation/xs:boolean(proj:front-page) eq true()]/@name
     return xdmp:apply($config:browse-facets, $config:RESPONSE/search:facet[@name = $front-page], ())}
    {xdmp:apply( $config:bootstrap )}
  </div>
};

declare function asc:result-title($uri as xs:string,$result as document-node())
as element(div)
{
  let $params := map:map()
  let $labels := map:put($params,"{http://marklogic.com/appservices/config}LABELS",$config:LABELS)
  let $title := xdmp:xslt-invoke($config:TRANSFORM-ABSTRACT-TITLE,$result,$params)
  return
  <div class="title">
    <a href="{ concat("/detail",encode-for-uri($uri),"?q=",if ($config:CONTEXT/*:q) then encode-for-uri($config:CONTEXT/*:q) else (),
               if ($config:CONTEXT/*:start) then concat("&amp;start=", encode-for-uri($config:CONTEXT/*:start)) else ()) }">
      <span class="result-title">{if ($title) then $title else <emphasis>[view item]</emphasis>}</span>
    </a>
  </div>
};
