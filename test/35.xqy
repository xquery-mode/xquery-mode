
(:~
 : This function includes an array if there are items to put in it.
 : If there are no such items, then it returns an empty sequence.
 : TODO EA-4? move to es: module
 :)
declare function northwind:extract-array(
  $path-to-property as item()*,
  $fn as function(*)
) as json:array?
{
  if (empty($path-to-property))
  then ()
  else json:to-array($path-to-property ! $fn(.))
};
