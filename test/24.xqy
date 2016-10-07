if ($facility-type = "GROUP") then
  (
   if ($mpf-group-entry/provider-tax-id/irs-number != $facility/c:ein) then
     "billing provider tax ID does not match EIN in MPF"
   else
     ()
  )
else
  (
   if ($mpf-entry/npi-api/npi-api != $provider/c:npi) then
     "rendering provider NPI does not match MPF npi-api record"
   else
     ())
