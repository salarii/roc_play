        app "exp"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.3.1/97mY3sUwo433-pcnEQUlMhn-sWiIf_J9bPhcAFZoqY4.tar.br" }
    imports [pf.Stdout] 
    provides [main] to pf

getVal = \  str  -> 
    when  Str.toDec  str is 
                Ok val ->  
                    val
                Err  _ -> 0.0dec


fun  = \ in  ->
    myset = Set.fromList [Tag1,  Tag2,  Tag3]
    

    indict =
        Dict.empty {}
        |> Dict.insert "key1" Tag1
        |> Dict.insert "key2" Tag2
        |> Dict.insert "key3" Tag3
        |> Dict.insert "key4" (Gang getVal("1.1"))
        
    content = Dict.get indict "key4"

    when content is 
        Ok (Gang val ) ->  Tag3
        Ok someTag -> 
            newSet = Set.remove  myset  someTag
            someTag
        _ -> Tag3
        

main =

    r= fun  3
    dbg  r
    Stdout.line "I'm a Roc application!"