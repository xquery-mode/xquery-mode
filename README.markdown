# xquery-mode

## Indentation

This mode support smart indentation based on syntax analysis.

Like any approach it have issues.  There some of them

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
