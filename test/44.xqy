
declare function rest-impl:conditions(
  $elem as element()*)
as element()*
{
  $elem[not(self::rest:param)
        and not(self::rest:uri-param)
        and not(self::rest:http)]
};
