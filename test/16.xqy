let $mpf-tin := if ($mpf-group) then $mpf-group/provider-tax-id[1]/irs-number/text() else $mpf-provider/provider-tax-id[1]/irs-number/text()
where $facility-npi != $mpf-facility-npi or $xxx != $yyy
      or $facility-tin != $mpf-tin
      or $provider-npi != $mpf-provider-npi
      or $name-classification != 'I' or not($mpf-provider-name)
return
  <tr>
