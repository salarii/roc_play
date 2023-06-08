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
    
    x = 10
    y = 10
    z = 10
    delta  = 2
    modCub  =
            Sim.makeCube x y z 0 
            |> Sim.modifyFieldCube  1 1 1 3
    str  = Sim.makeStringCube modCub { z : "\nz\n",y : "\ny\n" }
    
    cube  = Parsing.readCube  str

    #dbg  cube
    Stdout.line  ( str )
    #Stdout.line  (Sim.makeStringCube cube { z : "\n\n",y : "\n" } )
    #Stdout.line  "dadsa"
    

    
    
    
