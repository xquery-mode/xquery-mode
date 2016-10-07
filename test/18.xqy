xquery version "1.0-ml";

declare namespace c = "http://mcna.net/Schema/claims/1.0";

declare variable $claim-id as xs:string external := "10123935";

declare variable $claim := doc(concat('/claims/', $claim-id, '.xml'))/c:claim;

declare function local:validate-with-tpi($tpi as xs:string, $claim as element()) as xs:string* {

  let $access-point := $claim/c:access-point
  let $facility := $access-point/c:facility
  let $facility-type := $facility/c:type/text()
  let $provider := $access-point/c:provider

  let $mpf-entry := doc(concat('/tx-mpf/', substring($tpi, 1, 7), '.xml'))/group/provider[tpi = $tpi]
  let $mpf-group-membership := $mpf-entry/provider-group-membership[last()]
  let $group-tpi := $mpf-group-membership/group-tpisuffix-code
  let $mpf-group-entry := if ($group-tpi) then
                            doc(concat('/tx-mpf/', substring($group-tpi, 1, 7), '.xml'))/group/provider[tpi = $group-tpi]
                          else
                            ()
  return
    if ($facility-type = "GROUP") then
      (
       (: GROUP checks :)
       if ($mpf-group-membership) then
         ()
       else
         "claim indicates GROUP facility but TPI does not point to provider with provider-group-membership record",

       if ($mpf-entry/npi-api/npi-api != $provider/c:npi) then
         "rendering provider NPI does not match MPF npi-api record"
       else
         (),

       if ($mpf-group-membership/group-npi-api != $facility/c:npi) then
         "billing provider NPI in claim does not match NPI in MPF group membership record"
       else
         (),

       if ($mpf-group-entry/provider-tax-id/irs-number != $facility/c:ein) then
         "billing provider tax ID does not match EIN in MPF"
       else
         ()
      )
    else
      (
       (: INDIVIDUAL checks :)
       if ($mpf-group-membership) then
         "claim indicates INDIVIDUAL facility but TPI points to provider with provider-group-membership record"
       else
         (),

       if ($mpf-entry/npi-api/npi-api != $provider/c:npi) then
         "rendering provider NPI does not match MPF npi-api record"
       else
         ())
};

<claim-validations>
  {
    for $claim in subsequence(/c:claim, 1, 500)
    let $tpi := $claim/c:access-point/c:medicaid-id/text()
    let $errors := if ($tpi) then
                     local:validate-with-tpi($tpi, $claim)
                   else
                     ()
    return
      if ($errors) then
        <cv:claim id="{$claim/@c:id}">
          { for $message in $errors return <cv:error>{ $message }</cv:error> }
        </cv:claim>
      else
        ()
  }
</claim-validations>
