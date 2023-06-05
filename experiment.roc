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
    y = 16
    z = 10
    delta  = 2
    modCub  =
            Sim.makeCube x y z 0 
            |> Sim.modifyFieldCube  1 1 1 3
    #sol  = (Sim.calculateSolution  (Sim.makeCube x y z 0  ) modCub  delta {plus : 0, minus : 0 } 10 )
    
    sl  = Sim.circle  5  9  4   1
    
    sl1  = [{ x: 4, y: 9, z: 5 },{ x: 5, y: 8, z: 5 },{ x: 5, y: 9, z: 4 },{ x: 5, y: 9, z: 5 },{ x: 5, y: 9, z: 6 }
        ,{ x: 5, y: 10, z: 5 }
        ,{ x: 6, y: 9, z: 5 }]
    dbg   sl
    dbg  sl1 
    slice =   
        Sim.makeCube x y z 0
        |> Sim.setShape sl1  1
        |> Sim.setShape sl  1
        #|> Sim.sliceCube {x: All, y :Idx 9, z: All}
    #    |> Sim.modifyFieldCube  1 2  2  10
    #sq = makeSquare 3  3  5
    #
    #sol  = (Sim.calculateSolution  (Sim.makeCube 8 8 8 0  ) modCub  1 {plus : 0, minus : 0 } 100 )

    #Stdout.line  ( Sim.makeStringCube slice "\n\n" )  
    #Stdout.line  ( Sim.makeStringSq slice)
    Stdout.line  "dadsa"
    

    
    
    
