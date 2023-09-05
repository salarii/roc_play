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
    Sim.modifyFieldSq  sq  9 9   (Util.createNode  1 1 0)

forceCube = \ cubeX1, cubeY1, cubeZ1, cubeX2, cubeY2, cubeZ2, deltaT, cnt ->
    modifZ1  = Sim.modifyFieldCube  cubeZ1  1 1 1  (Util.createNode  1 1 0)
    {xField1 : cubeX1,  yField1 : cubeY1, zField1  : modifZ1, xField2 : cubeX2,  yField2 : cubeY2, zField2  : cubeZ2 } 

getRelevant = \ elem ->
    elem.value

createWorld = \ configList -> 
    List.walk  configList  []  serviceConfig

test  = \ clad,  mad ->
    mad clad
    
printCubes = \ cubes ->
    List.walk cubes  "" 
    ( \ state, cube   -> state
        |> Str.concat ( Sim.makeStringCube cube getRelevant  {y:"\n",z:"\n"} ) 
        |> Str.concat "\nTime + 1\n" )  
    
main =
    #line  simulation
    #orange = List.repeat (Util.createNode   0 1 0) 20
    #blue =  List.repeat (Util.createNode   0 1  0) 21
    #size  = 20
    
    # square simeulation
    #xOrange = Sim.makeSquare  size   (size + 1)   (Util.createNode   0 1 0)
    #yOrange = Sim.makeSquare  (size + 1)   size   (Util.createNode   0 1 0)
    #zBlue  = Sim.makeSquare  size   size   (Util.createNode   0 1 0)
    
    #result = Sim.xyVariationSim  xOrange yOrange zBlue forceSq 5  {zField  : [], xField : [],  yField : []}
    #result =  Sim.testFun zBlue forceSq
    #Stdout.line ( Sim.makeStringSq result getRelevant  "\n" )
    #Stdout.line ( Sim.makeStringCube result.zField getRelevant  {y:"\n",z:"\n"} )
    #Stdout.line ( Sim.makeStringCube result.xField getRelevant  {y:"\n",z:"\n"} )
    #Stdout.line ( Sim.makeStringCube result.yField getRelevant  {y:"\n",z:"\n"} )
    
    # cube  simulation
    size  = 4
    xOrange = Sim.makeCube size (size + 1) (size + 1) (Util.createNode   0 1 0)
    yOrange = Sim.makeCube (size + 1) size (size + 1) (Util.createNode   0 1 0)
    zOrange = Sim.makeCube (size + 1) (size + 1) size (Util.createNode   0 1 0)
    
    xBlue = Sim.makeCube size size size (Util.createNode   0 1 0)
    yBlue = Sim.makeCube size size size (Util.createNode   0 1 0)
    zBlue = Sim.makeCube size size size (Util.createNode   0 1 0)

    result = Sim.testFun  zBlue 
    Stdout.line ( Sim.makeStringCube result getRelevant  {y:"\n",z:"\n"} ) 
    
    #result = Sim.xyzVariationSim xOrange yOrange zOrange xBlue yBlue zBlue  forceCube 2  {xField1 : [],  yField1 : [], zField1  : [], xField2 : [],  yField2 : [], zField2  : [] }  
    #Stdout.line ( printCubes   result.yField2)   
    
    
