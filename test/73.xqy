declare function local:transmission($file-data as node() , $line-data as node(), $index as xs:integer, $icn as xs:string, $inbound-file-uri as xs:string) as node()
{
  (: ALERT: ONE BASED INDEXING as we expect to process this data within xslt or xquery.
     carries :next pointer :)
  <transmission xmlns="https://some.ns">
    <icn>{$icn}</icn>
    <remote-icn />
    <ids>
      {
        for $sid in tokenize($line-data/enc-xml:ids/text()," ")
        return <id>{$sid}</id>
      }
    </ids>
    <data-index>{$index}</data-index>
    <x12-index />
    <status>
      <code>pending</code>
      <timestamp>{$now}</timestamp>
      <cause>{$inbound-file-uri}</cause>
      <errors />
    </status>
    <status-history />
  </transmission>
};
