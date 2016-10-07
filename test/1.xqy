declare function local:get-provider-group-membership($mpf-provider as element(provider))
{
  (for $group-membership in $mpf-provider/provider-group-membership
