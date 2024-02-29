app "app"
    packages { pf: "platform/main.roc" }
    imports []
    provides [main] to pf


main : List F32,  List  F32  -> Str
main = \  lst1, lst2 ->
    Inspect.toStr lst2

# app "app"
#     packages { pf: "platform/main.roc" }
#     imports []
#     provides [main] to pf


# main : List F32 -> Str
# main = \lst ->
#     Inspect.toStr lst