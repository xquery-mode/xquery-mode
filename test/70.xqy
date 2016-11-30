document {
  <module>xquery version "1.0-ml";

          (: This module was generated by MarkLogic Entity Services.                          :)
          {es-codegen:comment("The source entity type document was " || $title || "-" || $version)}
          (:                                                                                  :)
          (: To use this module, examine how you wish to extract data from sources,           :)
          (: and modify the various extract-instance-{{X}} functions to                       :)
          (: create the instances you wish.                                                   :)
          {es-codegen:comment("Generated at timestamp: " || fn:current-dateTime())}
          module namespace {$prefix}
          = "{$base-uri}{$title}-{$version}";

          import module namespace es = "http://marklogic.com/entity-services"
          at "/MarkLogic/entity-services/entity-services.xqy";
          {
            (: Begin code generation block :)
            let $entity-type := $model=>map:get("definitions")=>map:get($entity-type-name)
            let $properties := map:get($entity-type, "properties")
            return
              concat($property-comment,
                     $function-call-string,
                     functx:pad-string-to-length("'" || $property-name || "',", " ", max((  (string-length($property-name)+4), 25) )+1 ),
                     $value,
                     ")&#10;")
              (: end code generation block :)
          }
  </module>/text()
}
