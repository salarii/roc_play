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
    
    
essenceDerivOp = \ listPlus, listMid, listMinus, delta -> 
    List.map3 listPlus listMid listMinus (\ plus , mid, minus -> (plus - 2*mid + minus )/(power2 delta))
    
secondDeriv = \ list, delta, edges ->
    timePlus1 = shift list Left edges.right 1
    timeMinus1 = shift list Right edges.left 1
    essenceDerivOp timePlus1 list timeMinus1 delta


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


makeStringCube = \ cube -> 
    List.walk cube  ""  ( \ str, sq  ->  Str.concat str (Str.concat ( makeStringSq sq ) "\n\n" ) )
     
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

secondDerivUpDown = \ lists,  delta, edges  -> 
    len = List.len (getListFromList  lists 0) 
    listScrollUp = List.dropLast ( List.prepend  lists (List.repeat edges.up len) )
    listScrollDown = List.dropFirst (List.append  lists (List.repeat edges.up len) )

    List.map3 listScrollUp lists listScrollDown  ( \   upList, midList, downList ->
                                                              dbg  upList
                                                              dbg  midList
                                                              dbg  downList
                                                              essenceDerivOp upList midList downList delta)
    

secondDerivZ  = \ cube, delta, edges  ->
    lenY = List.len  (getListFromList  cube 0)
    lenX = List.len  (getListFromList (getListFromList cube 0) 0)
    listZPlus = List.dropLast ( List.prepend  cube (makeSquare lenX lenY edges.plus  ) )
    listZMinus = List.dropFirst ( List.append  cube (makeSquare lenX lenY edges.minus ) )
    List.map3 listZPlus cube listZMinus  ( \   sqPlus, sq, sqMinus ->
        List.map3 sqPlus sq sqMinus( \   listPlus, list, listMinus ->
                                                              essenceDerivOp listPlus list listMinus delta))

solution = \ list, prevList, deltaTime, deltaSpace -> 
    dubDeriv = List.map (secondDeriv list  deltaSpace  {right : 0 , left: 0}) (\val  -> val * (power2  deltaTime )  ) 
    #dbg dubDeriv
    List.map3  dubDeriv  list  prevList  ( \ deriv, val, prevVal  -> deriv + 2.0 * val - prevVal  ) 
    
points  = 10  
iter  = 100 
cosMod = 3.14 / ( Num.toF32 points)
deltaConcreteTime = 0.1

calculateSolution  =  \ list, listPrev, cnt ->
    if cnt ==  0 then
        list
    else
        next = solution  list listPrev   deltaConcreteTime   cosMod 
        dbg (List.walk   list "" (\str, val -> Str.concat (Str.concat str  " ") ( Num.toStr val ) ) )
        calculateSolution  next list  ( cnt  - 1 )
   

   
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
main =
    
    cube  = makeCube 3 3 3 1 
    modCub = modifyFieldCube cube  0 1  1  0
    #sq = makeSquare 3  3  5
    #slice = sliceCube  modCub  {x: All, y :Idx 1, z: All}

    Stdout.line  ( makeStringCube modCub)
    #Stdout.line  ( makeStringSq slice)
    
    

    
    
    
