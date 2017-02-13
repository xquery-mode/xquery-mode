# xquery-mode

This is a fork of the original xquery-mode.

## Indentation

Indentation engine was fully re-engineered.  Current implementation
includes syntax analyzer which can indent line or region based on
context.

To enable this feature please add following lines to your config:

```elisp
(setq xquery-mode-indent-style 'native)
```

To indent current line please press `TAB`.  To indent whole buffer
please press `C-x h TAB`.

Like any approach it has issues.  There some of them

#### Tabs

Literally I don't know how to make this works.

#### Comments

Current algorithm don't support look ahead analysis.  Comments doesn't
terminate expressions.  Their indentation should be calculated from
next syntax construction.

So you will get this

```xquery
let $entry-count := fn:count($entries)
                    (: get transaction-size from policy :)
let $name := fn:data(info:ticket($ticket-id)/info:policy-name)
let $total-transactions := ceiling($entry-count div $transaction-size)
                           (: set total documents and total transactions so UI displays collecting :)
let $set-total := infodev:ticket-set-total-documents($ticket-id, $entry-count)
```

when you expect this

```xquery
let $entry-count := fn:count($entries)
(: get transaction-size from policy :)
let $name := fn:data(info:ticket($ticket-id)/info:policy-name)
let $total-transactions := ceiling($entry-count div $transaction-size)
(: set total documents and total transactions so UI displays collecting :)
let $set-total := infodev:ticket-set-total-documents($ticket-id, $entry-count)
```
