app "app"
    packages { pf: "platform/main.roc" }
    imports []
    provides [main] to pf


main : List F32 -> Str
main = \  lst ->
    "sfdfsfsdfds "

# app "app"
#     packages { pf: "platform/main.roc" }
#     imports []
#     provides [main] to pf


# main : List F32 -> Str
# main = \lst ->
#     Inspect.toStr lst