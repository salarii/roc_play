        app "exp"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.3.1/97mY3sUwo433-pcnEQUlMhn-sWiIf_J9bPhcAFZoqY4.tar.br" }
    imports [pf.Stdout, Parsing] 
    provides [main] to pf


createGold = \ gold, cnt, alreadyCreated ->  
    if cnt == 0 then
        alreadyCreated
    else 
        createGold gold (cnt - 1) (List.append  alreadyCreated { gold : gold })

getFromList = \ list, cnt  ->
    when List.get list cnt  is 
        Ok val -> val
        Err _ -> 0.0f32


serviceConfig = \ created, config ->
    if Parsing.checkIfPresent  config  (Set.fromList [Gold, Nodes ]) == Bool.true then
        goldConf = Parsing.getTagFromConfig Gold config 
        nodeConf = Parsing.getTagFromConfig Nodes config
        p = (Num.floor ((getFromList nodeConf.vals  0 ) ))
        createGold (getFromList goldConf.vals  0 ) 5 created# (Num.round ((getFromList nodeConf.vals  0 ) ) )   created
        
    else
        created


createWorld = \ configList -> 
    List.walk  configList  []  serviceConfig


main =
    #p  = Num.toDec ( Str.toF32  "50.4"  )
    #dbg Num.floor  1.3
    dbg  1.2
    #eton =  Parsing.setupEnv  "nodes 50  \n gold  0  \n "
    #dbg createWorld (eton.accepted )

    Stdout.line "I'm a Roc application!"