
return
  if (empty($type))
  then
    $strval
  else
    (: This is crude, but avoids the need for the xdmp-eval privilige. :)
    if ($type = "NCName") then $value cast as xs:NCName
    else if ($type = "NMTOKEN") then $value cast as xs:NMTOKEN
    else if ($type = "NMTOKENS") then $value cast as xs:NMTOKENS
    else if ($type = "Name") then $value cast as xs:Name
    else if ($type = "QName") then $value cast as xs:QName
    else if ($type = "anyURI") then $value cast as xs:anyURI
    else if ($type = "base64Binary") then $value cast as xs:base64Binary
    else if ($type = "boolean") then $value cast as xs:boolean
    else if ($type = "byte") then $value cast as xs:byte
    else if ($type = "date") then $value cast as xs:date
    else if ($type = "dateTime") then $value cast as xs:dateTime
    else if ($type = "decimal") then $value cast as xs:decimal
    else if ($type = "double") then $value cast as xs:double
    else if ($type = "duration") then $value cast as xs:duration
    else if ($type = "float") then $value cast as xs:float
    else if ($type = "gDay") then $value cast as xs:gDay
    else if ($type = "gMonth") then $value cast as xs:gMonth
    else if ($type = "gMonthDay") then $value cast as xs:gMonthDay
    else if ($type = "gYear") then $value cast as xs:gYear
    else if ($type = "gYearMonth") then $value cast as xs:gYearMonth
    else if ($type = "hexBinary") then $value cast as xs:hexBinary
    else if ($type = "int") then $value cast as xs:int
    else if ($type = "integer") then $value cast as xs:integer
    else if ($type = "language") then $value cast as xs:language
    else if ($type = "long") then $value cast as xs:long
    else if ($type = "negativeInteger") then $value cast as xs:negativeInteger
    else if ($type = "nonNegativeInteger") then $value cast as xs:nonNegativeInteger
    else if ($type = "nonPositiveInteger") then $value cast as xs:nonPositiveInteger
    else if ($type = "normalizedString") then $value cast as xs:normalizedString
    else if ($type = "positiveInteger") then $value cast as xs:positiveInteger
    else if ($type = "short") then $value cast as xs:short
    else if ($type = "time") then $value cast as xs:time
    else if ($type = "token") then $value cast as xs:token
    else if ($type = "unsignedByte") then $value cast as xs:unsignedByte
    else if ($type = "unsignedInt") then $value cast as xs:unsignedInt
    else if ($type = "unsignedLong") then $value cast as xs:unsignedLong
    else if ($type = "unsignedShort") then $value cast as xs:unsignedShort
    else if ($type = "string") then $value cast as xs:string
    else
      (rest-impl:log(concat("Invalid type specified in validation: ", $type)),
       error($rest-impl:INVALIDTYPE, $type))
