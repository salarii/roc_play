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


forceSq = \ sq, deltaT, cnt ->
    sq


getRelevant = \ elem ->
    elem.value

createWorld = \ configList -> 
    List.walk  configList  []  serviceConfig

test  = \ clad,  mad ->
    mad clad
main =
    
    orange = List.repeat (Util.createNode   0 5 0) 20
    blue =  List.repeat (Util.createNode   0 1  0) 21
    
    xOrange = Sim.makeSquare  2   3   (Util.createNode   0 5 0)
    yOrange = Sim.makeSquare  3   2   (Util.createNode   0 5 0)
    zBlue  = Sim.makeSquare  2   2   (Util.createNode   0 5 0)
    
    result = Sim.xyVariationSim  xOrange yOrange zBlue forceSq 3  {zField  : [], xField : [],  yField : []}
    
    #orangeCalc = Sim.lineMotion orange  blue force {front : (Util.createNode   0 1 0), back : (Util.createNode   0 1 0) } 40 []
    Stdout.line ( Sim.makeStringCube result.zField getRelevant  {y:"\n",z:"\n"} )

    

    
    
    
