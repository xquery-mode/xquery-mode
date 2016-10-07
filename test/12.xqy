{
  for $tpi in fn:subsequence()
  let $provider-id := $tpi/../../provider
  order by $provider-id/id
