let $mpf-group-entry := if ($group-tpi) then
                          doc(concat('/tx-mpf/', substring($group-tpi, 1, 7), '.xml'))/group/provider[tpi = $group-tpi]
                        else
                          ()
return
