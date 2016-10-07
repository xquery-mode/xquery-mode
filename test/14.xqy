return
  let $mpf-tin := if ($mpf-group)
                  then
                    $mpf-group/provider-tax-id[1]/irs-number/text()
                  else
                    $mpf-provider/provider-tax-id[1]/irs-number/text()
