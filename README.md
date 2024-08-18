# Elm Bindings for [Forester](http://www.jonmsterling.com/jms-005P.xml)

## What about using [Hypermedia instead of a JSON API](https://htmx.org/essays/hypermedia-apis-vs-data-apis)?

Forester is already hypermedium. The fact that we are serialising to JSON is an
implementation detail. I have opted for faithfully representing forester types
in the elm frontend for maximum expressiveness on the frontend. Instead of
compiling forester to HTML and using the browser as a "raw" hypermedia client,
we can effectively use elm to implement a custom hypermedia client model for forester.

## Notes

https://mirage.github.io/repr/repr/Repr/index.html#json-converters

Repr serializes constructors with multiple arguments into heterogeneous lists,
for example:

```ocaml
Rel of mode * polarity * Rel.t * 'var addr_expr
```

```json
{ "Rel": 
    [
        "Paths",
        "Outgoing",
        "org.forester.rel.transclusion",
        {
            "Addr": {
                "User_addr": "asdf"
            }
        }
    ]
}
```

How to decode:

Create an auxilliary type with one constructor per argument:

```elm
type Relpart var
    = M Mode
    | P Polarity
    | R Rel
    | AE (Addr_expr var)
```

Decode into a list of parts and only accept lists that match the constructor:
```elm
relpart : Decoder var -> Decoder (Relpart var)
relpart var =
    oneOf
        [ mode |> map M
        , polarity |> map P
        , string |> map R
        , addr_expr var |> map AE
        ]

rel : Decoder var -> Decoder (Expr var)
rel v =
    list (relpart v)
        |> andThen
            (\parts ->
                case parts of
                    [ M m, P pol, R r, AE e ] ->
                        succeed (Rel m pol r e)

                    _ ->
                        fail "Failed to decode Rel"
            )
```
