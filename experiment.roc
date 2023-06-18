        app "exp"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.3.1/97mY3sUwo433-pcnEQUlMhn-sWiIf_J9bPhcAFZoqY4.tar.br" }
    imports [pf.Stdout, Parsing, Sim, Util] 
    provides [main] to pf


createGold = \ gold, cnt ->   
    elem = List.repeat gold   (Num.round (Util.getFromList cnt 1) )
    List.repeat  elem  (Num.round(Util.getFromList cnt 0))


serviceConfig = \ created, config ->
    if Parsing.checkIfPresent  config  (Set.fromList [Gold, Nodes ]) == Bool.true then
        goldConf = Parsing.getTagFromConfig Gold config 
        nodeConf = Parsing.getTagFromConfig Nodes config
        value = Util.getFromList goldConf.vals  0  
        createGold value nodeConf.vals 
    else
        created


createWorld = \ configList -> 
    List.walk  configList  []  serviceConfig

test  = \ clad,  mad ->
    mad clad
main =
    
    orange = List.repeat 0 200
    blue = (List.replace   (List.repeat 0 201)  50  0).list
    
    orangeCalc = Sim.lineMotion orange  blue  40 []
    Stdout.line ( Sim.makeStringSq orangeCalc  "\n" )
    #Stdout.line  (Sim.makeStringCube cube { z : "\n\n",y : "\n" } )
    #Stdout.line  "dadsa"
    

    
    
    
