    app "exp"

    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br" }
    imports [pf.Stdout, Parsing, Sim, Util]
    provides [main] to pf


#serviceConfig = \ created, config ->
#    if Parsing.checkIfPresent  config  (Set.fromList [Gold, Nodes ]) == Bool.true then
#        goldConf = Parsing.getTagFromConfig Gold config 
#        nodeConf = Parsing.getTagFromConfig Nodes config
#        value = Util.getFromList goldConf.vals  0  0.0f32 
#        createGold value nodeConf.vals 
#    else
#        created

force = \ list, deltaT, cnt ->
    (List.replace  list  10  (Util.createNode   1 1 0)).list


forceSq = \ sq, deltaT, cnt ->
    Sim.modifyFieldSq  sq  2 2   (Util.createNode  1 1 0)

forceSq2 = \ sq, deltaT, cnt ->
    Sim.modifyFieldSq  sq  9 9  (Util.createNodeAni  1 1 0 0 0)

    
forceCube = \ cubeX1, cubeY1, cubeZ1, cubeX2, cubeY2, cubeZ2, deltaT, cnt ->
    modifZ2  = Sim.modifyFieldCube  cubeZ2  1 1 1  (Util.createNode  1 1 0)
    {xField1 : cubeX1,  yField1 : cubeY1, zField1  : cubeZ1, xField2 : cubeX2,  yField2 : cubeY2, zField2  : modifZ2 } 



getOmega = \ elem ->
    Num.toStr elem.omega.x 
    |> Str.concat ","
    |> Str.concat (Num.toStr elem.omega.y)
    |> Str.concat ","
    |> Str.concat (Num.toStr elem.omega.z)
    
#createWorld = \ configList -> 
#    List.walk  configList  []  serviceConfig

test  = \ clad,  mad ->
    mad clad
    
printCubes = \ cubes ->
    List.walk cubes  "" 
    ( \ state, cube   -> state
        |> Str.concat ( Util.makeStringCube cube Util.getRelevant  {y:"\n",z:"\n"} ) 
        |> Str.concat "\nTime + 1\n" )  
    
main =
    #line  simulation
    #orange = List.repeat (Util.createNode   0 1 0) 20
    #blue =  List.repeat (Util.createNode   0 1  0) 21
    #size  = 20
    
    # square simeulation
    #xOrange = Sim.makeSquare  size   (size + 1)   (Util.createNode   0 1 1000000)
    #yOrange = Sim.makeSquare  (size + 1)   size   (Util.createNode   0 1 1000000)
    #zBlue  = Sim.makeSquare  size   size   (Util.createNode   0 1 0)

    #result = Sim.xyVariationSim  xOrange yOrange zBlue forceSq 4  {zField  : [], xField : [],  yField : []}
    #result =  Sim.testFun zBlue forceSq
    #Stdout.line ( Sim.makeStringSq result getRelevant  "\n" )
    #Stdout.line ( Sim.makeStringCube result.zField getRelevant  {y:"\n",z:"\n"} )
    #Stdout.line ( Sim.makeStringCube result.xField getRelevant  {y:"\n",z:"\n"} )
    #Stdout.line ( Sim.makeStringCube result.yField getRelevant  {y:"\n",z:"\n"} )
    
    # square simulation 2
    #size  = 20
    #xOrange = Sim.makeSquare  size   (size + 1)   (Util.createNodeAni   0 1 10 10 10)
    #yOrange = Sim.makeSquare  (size + 1)   size   (Util.createNodeAni   0 1 10 10 10)
    #zBlue  = Sim.makeSquare  size   size   (Util.createNodeAni   0 1 0 0 0)
    
    #boxi = Sim.pmlIzeSq zBlue 4  1 1
    
    #Stdout.line ( Sim.makeStringSq boxi getOmega  "\n" )
    
    #result = Sim.xyVariationSim2  xOrange yOrange zBlue forceSq2 10 {zField  : [], xField : [],  yField : []}
    #result =  Sim.testFun zBlue forceSq
    #Stdout.line ( Sim.makeStringSq result getRelevant  "\n" )
    #Stdout.line ( Sim.makeStringCube result.zField getRelevant  {y:"\n",z:"\n"} )
    #Stdout.line ( Sim.makeStringCube result.xField getRelevant  {y:"\n",z:"\n"} )
    #Stdout.line ( Sim.makeStringCube result.yField getRelevant  {y:"\n",z:"\n"} )
    
    
    
    # cube  simulation
    
    
    size  = 3
    
    xOrange = Sim.makeCube size (size + 1) (size + 1) (Util.createNode   0 1 0)
    yOrange = Sim.makeCube (size + 1) size (size + 1) (Util.createNode   0 1 0)
    zOrange = Sim.makeCube (size + 1) (size + 1) size (Util.createNode   0 1 0)
    
    xBlue = Sim.makeCube (size +1 ) size size (Util.createNode   0 1 0)
    yBlue = Sim.makeCube size (size + 1) size (Util.createNode   0 1 0)
    zBlue = Sim.makeCube size size (size + 1) (Util.createNode   0 1 0)

    xBlue = Sim.makeCube size  size size (Util.createNodeAni   0 1 0 0 0)
    
    #Sim.pmlIzeCube  xBlue  2  1 1
    #|> Util.makeStringCube  getOmega  {y:"\n",z:"\n"}
    #|> Stdout.line 
#    xModif= Sim.modifyFieldCube  zBlue 2 2 2 (Util.createNode   1 1 0)

    #result =  Sim.cubeElemOperation  xModif zBlue Sim.minusElemOp
    #Stdout.line ( Sim.makeStringCube result getRelevant  {y:"\n",z:"\n"} ) 
    
    #result = Sim.xyzVariationSim  {deltaSpace : 1, deltaT : 0.1} xOrange yOrange zOrange xBlue yBlue zBlue  forceCube 2  {xField1 : [],  yField1 : [], zField1  : [], xField2 : [],  yField2 : [], zField2  : [] }  
    #Stdout.line ( printCubes   result.zField2)
    
    
