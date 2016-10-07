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
