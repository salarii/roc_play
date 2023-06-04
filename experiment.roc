        app "exp"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.3.1/97mY3sUwo433-pcnEQUlMhn-sWiIf_J9bPhcAFZoqY4.tar.br" }
    imports [pf.Stdout, Parsing] 
    provides [main] to pf


getFromList = \ list, cnt  ->
    when List.get list cnt  is 
        Ok val -> val
        Err _ -> 0.0f32


getListFromList = \ list, cnt  ->
    when List.get list cnt  is 
        Ok val -> val
        Err _ -> []

createGold = \ gold, cnt ->   
    elem = List.repeat gold   (Num.round (getFromList cnt 1) )
    List.repeat  elem  (Num.round(getFromList cnt 0))


serviceConfig = \ created, config ->
    if Parsing.checkIfPresent  config  (Set.fromList [Gold, Nodes ]) == Bool.true then
        goldConf = Parsing.getTagFromConfig Gold config 
        nodeConf = Parsing.getTagFromConfig Nodes config
        value = getFromList goldConf.vals  0  
        createGold value nodeConf.vals 
    else
        created


createWorld = \ configList -> 
    List.walk  configList  []  serviceConfig

increasedList = \ cnt, list -> 
        if cnt == 0 then
            list
        else
            increasedList (cnt - 1) (List.append  list cnt) 
         
centerList  = \ cnt ->
        List.map ( increasedList cnt  [] ) (\ elem ->  elem - cnt / 2 )


modList = \ list, mod ->
    List.map list  (\ elem -> (Num.toF32 elem) * mod )

power2 = \ cnt -> 
    cnt * cnt


shift = \ list,  direction, fill, cnt ->
    if cnt == 0 then
        list
    else
        if direction == Right then
            List.dropLast list
            |> List.prepend fill 
            |> shift direction  fill (cnt - 1) 
        else if direction == Left then
            List.dropFirst list
            |> List.append fill
            |> shift direction  fill (cnt - 1)
        else 
            list
    
makeSquare = \ x, y, val ->
    makeSquareRec x y val []
    
makeSquareRec = \ x, y, val, list  ->
    if y == 0 then
        list
    else
        makeSquareRec x (y - 1) val (List.append  list (List.repeat  val  x ))

makeCube = \ x, y, z, val  ->
    makeCubeRec x y z val []

makeCubeRec = \ x, y, z, val ,list  ->
    if z == 0 then
        list
    else
        makeCubeRec  x y  (z - 1) val (List.append list (makeSquare x y val ) )


makeStringCube = \ cube, separate-> 
    List.walk cube  ""  ( \ str, sq  ->  Str.concat str (Str.concat ( makeStringSq sq ) separate ) )
     
makeStringSq =  \ square -> 
    List.walk square  "" (\ str, list -> List.walk list (Str.concat  str "\n") (\strIn, val -> Str.concat (Str.concat strIn  " ") ( Num.toStr val ) ) )

modifyFieldSq  = \ square, x, y, val ->
    getListFromList square y
    |> List.replace x val 
    |> (\ updated -> (List.replace square y updated.list).list  )

modifyFieldCube  = \ cube, x, y, z, val ->
    getListFromList cube z
    |> getListFromList y
    |> List.replace x val 
    |> (\ updated -> (List.replace cube z ( List.replace (getListFromList cube z) y updated.list  ).list   ).list  )

essenceDerivOp = \ listPlus, listMid, listMinus, delta -> 
    List.map3 listPlus listMid listMinus (\ plus , mid, minus -> (plus - 2*mid + minus )/(power2 delta))
    
opDerivXlist = \ list, delta, edges, op ->
    timePlus = shift list Left edges.plus 1
    timeMinus = shift list Right edges.minus 1
    op timePlus list timeMinus delta

opDerivXCube = \ cube, delta, edges, op ->
    List.walk cube [] (
         \ out ,sq -> 
            out
            |> List.append (List.walk sq [] (
                \ outList, list ->
                    outList
                    |> List.append  (opDerivXlist list delta edges op) )))
                    
opDerivYSq = \ lists,  delta, edges, op  -> 
    len = List.len (getListFromList  lists 0) 
    listYPlus = 
        lists 
        |> List.prepend (List.repeat edges.plus len)
        |> List.dropLast
    listYMinus = 
        lists
        |> List.append (List.repeat edges.minus len)
        |> List.dropFirst 

    List.map3 listYPlus lists listYMinus  ( \   listPlus, list, listMinus ->
                                                              op listPlus list listMinus delta)
    
opDerivYCube = \ cube, delta, edges, op ->
    List.walk cube [] ( \ out ,sq -> List.append  out  (opDerivYSq sq delta edges op) )

opDerivZCube  = \ cube, delta, edges, op  ->
    lenY = 
        getListFromList  cube 0
        |> List.len
          
    lenX = 
        getListFromList cube 0
        |> getListFromList 0
        |> List.len
          
    listZPlus = 
        cube
        |> List.prepend (makeSquare lenX lenY edges.plus  )
        |> List.dropLast 
    listZMinus =
        cube
        |> List.append (makeSquare lenX lenY edges.minus )
        |> List.dropFirst 
         
    List.map3 listZPlus cube listZMinus ( 
        \ sqPlus, sq, sqMinus -> List.map3 sqPlus sq sqMinus(
                \ listPlus, list, listMinus ->
                        op listPlus list listMinus delta) )

poissonDerivOp = \ listPlus, listMid, listMinus, delta -> 
    List.map3 listPlus listMid listMinus (\ plus , mid, minus -> (plus  + minus )/(power2 delta))
    

opCubes = \ leftCube, rightCube, op ->
    List.map2 leftCube rightCube (
        \ leftSq, rightSq -> List.map2  leftSq rightSq (
            \ leftList, rightList -> List.map2 leftList rightList op ) )

opCube = \ cube, val, op ->
    List.map cube (
        \ sq -> List.map  sq (
            \ list -> List.map list ( \ listVal ->  op listVal val ) ) )


addOp = \ left, right ->
     left + right

minusOp = \ left, right ->
     left - right

divideOp = \ left, right ->
     left / right

solution = \ cube, cubeCharge, delta, edges -> 
    opDerivXCube cube  1 {plus : 0, minus : 0}  poissonDerivOp
    |> opCubes (opDerivYCube cube  1 {plus : 0, minus : 0}  poissonDerivOp) addOp
    |> opCubes (opDerivZCube cube  1 {plus : 0, minus : 0}  poissonDerivOp ) addOp
    |> opCubes cubeCharge minusOp
    |> opCube ( 6 * (power2 delta)) divideOp
    
#    dubDeriv = List.map (secondDeriv list  deltaSpace  {right : 0 , left: 0}) (\val  -> val * (power2  deltaTime )  ) 
    #dbg dubDeriv
#    List.map3  dubDeriv  list  prevList  ( \ deriv, val, prevVal  -> deriv + 2.0 * val - prevVal  ) 
    

    
    
points  = 10  
iter  = 100 
cosMod = 3.14 / ( Num.toF32 points)
deltaConcreteTime = 0.1

calculateSolution  =  \ cube, cubeCharge, delta, edges, cnt ->
    if cnt ==  0 then
        cube
    else
        solution  cube cubeCharge delta edges 
        |> calculateSolution cubeCharge delta edges ( cnt  - 1 )
   

   
sliceCube = \ cube, slices ->
    when slices  is 
        { x : Idx xVal, y : All, z : All }  -> List.walk cube  [] ( \out ,sq  -> List.append out ( List.walk sq [] ( \ outList, list-> List.append  outList (getFromList list xVal)  ) ) )   
        { x : All, y : Idx yVal, z : All }  -> List.walk cube [] ( \out ,sq -> List.append out ( getListFromList sq  yVal ) )    
        { x : All, y : All, z : Idx zVal }  -> (getListFromList cube zVal)    
        { x : Idx xVal, y : Idx yVal, z : All }  -> [List.walk cube  [] ( \out, sq -> List.append out (getFromList (getListFromList sq yVal) xVal  ) ) ] 
        { x : Idx xVal, y : All, z : Idx zVal  }  -> [List.walk (getListFromList cube zVal) [] ( \list, out -> List.append out ( getFromList list  xVal ) )]    
        { x : All, y : Idx yVal, z : Idx zVal  }  -> [(getListFromList (getListFromList cube zVal) yVal)]     
        { x : Idx xVal, y : Idx yVal, z : Idx zVal  }  -> [[ getFromList (getListFromList (getListFromList cube zVal) yVal) xVal  ]]
        _ ->  []

test  = \ clad,  mad ->
    mad clad
main =
    
    modCub  =
        makeCube 3 3 3 0 
        |> modifyFieldCube  1 1  1  -101
        |> modifyFieldCube  1 2  2  10
    #sq = makeSquare 3  3  5
    #slice = sliceCube  modCub  {x: All, y :Idx 1, z: All}
    sol  = (calculateSolution  (makeCube 3 3 3 0 ) modCub  1 {plus : 0, minus : 0 } 100 )
    solutionD =
        
        opDerivXCube sol 1 {plus : 0, minus : 0}  essenceDerivOp
        |> opCubes (opDerivYCube sol  1 {plus : 0, minus : 0}  essenceDerivOp) addOp
        |> opCubes (opDerivZCube sol  1 {plus : 0, minus : 0}  essenceDerivOp ) addOp
    
    Stdout.line  ( makeStringCube  solutionD "\n\n" )
    #Stdout.line  ( makeStringSq slice)
    
    

    
    
    
