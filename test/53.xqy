(:~
 : Creates a map:map instance from some source document.
 : @param $source-node  A document or node that contains
 :   data for populating a Customer
 : @return A map:map instance with extracted data and
 :   metadata about the instance.
 :)
declare function northwind:extract-instance-OrderDetail(
  $source-node as node()
) as map:map
{
  (: if this $source-node is a reference to another instance, then short circuit.     :)
  if (empty($source-node/element()/*))
  then json:object()
       =>map:with('$type', 'OrderDetail')
       =>map:with('$ref', $source-node/OrderDetail/text())
       =>map:with('$attachments', $source-node)
       (: otherwise populate this instance :)
  else json:object()
       (: The following line identifies the type of this instance.  Do not change it.      :)
       =>map:with('$type', 'OrderDetail')
       (: The following line adds the original source document as an attachment.           :)
       =>map:with('$attachments', $source-node)
       (: If the source document is JSON, remove the previous line and replace it with     :)
       (: =>map:with('$attachments', xdmp:quote($source-node))                             :)
       (: because this implementation uses an XML envelope.                                :)
       (:                                                                                  :)
       (: The following code populates the properties of the                               :)
       (: 'Customer' entity type                                                           :)
       (: Ensure that all of the property paths are correct for your source data.          :)
       (: The general pattern is                                                           :)
       (: =>map:with('keyName', casting-function($source-node/path/to/data))               :)
       (: but you may also wish to convert values                                          :)
       (: =>map:with('dateKeyName',                                                        :)
       (:       xdmp:parse-dateTime("[Y0001]-[M01]-[D01]T[h01]:[m01]:[s01].[f1][Z]",       :)
       (:       $source-node/path/to/data/in/the/source))                                  :)
       (: You can also implement lookup functions,                                         :)
       (: =>map:with('lookupKey',                                                          :)
       (:       cts:search( collection('customers'),                                       :)
       (:           string($source-node/path/to/lookup/key))/id                            :)
       (: or populate the instance with constants.                                         :)
       (: =>map:with('constantValue', 10)                                                  :)
       (: The output of this function should structurally match the output of              :)
       (: es:model-get-test-instances($model)                                              :)
       (:                                                                                  :)
       =>   map:with('Order_ID',             xs:string($source-node/OrderDetail/Order_ID))
       =>es:optional('Order_Date',            xs:date($source-node/OrderDetail/Order_Date))
       =>es:optional('Product_Name',                xs:string($source-node/OrderDetail/Product_Name))
       (: The following property is a local reference.                                     :)
       =>es:optional('Customer_Name',                    xs:string($source-node/OrderDetail/Customer_Name))

};
