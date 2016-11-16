
(:~
 : Turns an entity instance into an XML structure.
 : This out-of-the box implementation traverses a map structure
 : and turns it deterministically into an XML tree.
 : Using this function as-is should be sufficient for most use
 : cases, and will play well with other generated artifacts.
 : @param $entity-instance A map:map instance returned from one of the extract-instance
 :    functions.
 : @return An XML element that encodes the instance.
 :)
declare function northwind:instance-to-canonical-xml(
  $entity-instance as map:map
) as element()
{
  (: Construct an element that is named the same as the Entity Type :)
  element { map:get($entity-instance, "$type") }  {
    if ( map:contains($entity-instance, "$ref") )
    then map:get($entity-instance, "$ref")
    else
      for $key in map:keys($entity-instance)
      let $instance-property := map:get($entity-instance, $key)
      where ($key castable as xs:NCName and $key ne "$type")
      return
        typeswitch ($instance-property)
        (: This branch handles embedded objects.  You can choose to prune
           an entity's representation of extend it with lookups here. :)
        case json:object+
          return
            for $prop in $instance-property
            return element { $key } { northwind:instance-to-canonical-xml($prop) }
        (: An array can also treated as multiple elements :)
        case json:array
          return
            for $val in json:array-values($instance-property)
            return
              if ($val instance of json:object)
              then element { $key } { northwind:instance-to-canonical-xml($val) }
              else element { $key } { $val }
        (: A sequence of values should be simply treated as multiple elements :)
        case item()+
          return
            for $val in $instance-property
            return element { $key } { $val }
        default return element { $key } { $instance-property }
  }
};
