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


force = \ list, deltaT, cnt ->
    (List.replace  list  10  (Util.createNode   1 1 0)).list

getRelevant = \ elem ->
    elem.value

createWorld = \ configList -> 
    List.walk  configList  []  serviceConfig

test  = \ clad,  mad ->
    mad clad
main =
    
    orange = List.repeat (Util.createNode   0 5 0) 20
    blue =  List.repeat (Util.createNode   0 1  0) 21
    
    orangeCalc = Sim.lineMotion orange  blue force {front : (Util.createNode   0 1 0), back : (Util.createNode   0 1 0) } 40 []
    Stdout.line ( Sim.makeStringSq orangeCalc getRelevant  "\n" )
    #Stdout.line  (Sim.makeStringCube cube { z : "\n\n",y : "\n" } )
    #Stdout.line  "dadsa"
    

    
    
    
