interface Sim
    exposes [modifyFieldCube, calculateSolution, makeCube, makeStringCube
            ,makeStringSq, sliceCube, check, sphere, setShape, lineMotion ]
    imports [Util]


increasedList = \ cnt, list -> 
        if cnt == 0 then
            list
        else
            increasedList (cnt - 1) (List.prepend  list (cnt - 1)) 
         
shiftList  = \ list, center  ->
        List.map list (\ elem ->  elem + center )


modList = \ list, mod ->
    List.map list  (\ elem -> (Num.toF32 elem) * mod )

mulListElem = \ list, mod ->
    List.map list  (\ elem -> {elem  &  value : elem.value * mod } )

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


makeStringCube = \ cube, getRelevant, separator-> 
    List.walk cube  ""  ( \ str, sq  ->  Str.concat str (Str.concat ( makeStringSq sq getRelevant separator.y ) separator.z ) )
     
makeStringSq =  \ square, getRelevant,separator -> 
    List.walk square  "" (\ str, list ->
                            List.walk list str (\strIn, val -> Str.concat (Str.concat strIn  " ") ( Num.toStr (getRelevant val) ) )
                            |> Str.concat  separator )
    
modifyFieldSq  = \ square, x, y, val ->
    Util.getListFromList square y
    |> List.replace x val 
    |> (\ updated -> (List.replace square y updated.list).list  )

removeFirstSq = \ square ->
    square
    |> List.dropFirst
    |> List.map List.dropFirst

removeFirstCube = \ cube ->
    cube


mulSqElem = \ square, mod ->
    List.map square  (\ list -> mulListElem list mod )

mulCubeElem = \ cube, mod ->
    List.map cube  (\ sq -> mulSqElem sq mod )

sqElemOperation = \ squareL, squareR, op ->
    List.map2 squareL squareR (\ listL, listR  -> List.map2 listL listR  op )


cubeElemOperation = \ cubeL, cubeR, op ->
    List.map2 cubeL cubeR  (\ sqL, sqR  -> sqElemOperation sqL sqR  op )

addFirstBackSq = \ square, first, back ->
    len = List.len (Util.getListFromList  square 0)
    square
    |> List.prepend (List.repeat first len)
    |> List.prepend (List.repeat back len)


modifyFieldCube  = \ cube, x, y, z, val ->
    Util.getListFromList cube z
    |> Util.getListFromList y
    |> List.replace x val 
    |> (\ updated -> (List.replace cube z ( List.replace (Util.getListFromList cube z) y updated.list  ).list   ).list  )

essenceDerivOp = \ listPlus, listMid, listMinus, delta -> 
    List.map3 listPlus listMid listMinus (\ plus , mid, minus -> (plus - 2*mid + minus )/(power2 delta))
    
poissonDerivOp = \ listPlus, listMid, listMinus, delta -> 

    List.map3 listPlus listMid listMinus (\ plus , mid, minus -> (plus  + minus )/(power2 delta))
    
deriv1Op = \ listPlus, listMid, listMinus, delta ->   
    List.map3 listPlus listMid listMinus (\ plus , mid, minus -> (mid - minus )/delta)

deriv1ElemOp = \ listPlus, listMid, listMinus, delta ->   
    List.map3 listPlus listMid listMinus (\ plus , mid, minus -> { mid & value : (mid.value - minus.value )/delta  })


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

opDerivXSq = \ lists,  delta, edges, op  -> 
    List.walk lists [] (
         \ out ,list -> 
            out
            |> List.append  (opDerivXlist list delta edges op) )
                    
opDerivYSq = \ lists,  delta, edges, op  -> 
    len = List.len (Util.getListFromList  lists 0) 
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
        Util.getListFromList  cube 0
        |> List.len
          
    lenX = 
        Util.getListFromList cube 0
        |> Util.getListFromList 0
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



opCubes = \ leftCube, rightCube, op ->
    List.map2 leftCube rightCube (
        \ leftSq, rightSq -> List.map2  leftSq rightSq (
            \ leftList, rightList -> List.map2 leftList rightList op ) )

opCube = \ cube, val, op ->
    List.map cube (
        \ sq -> List.map  sq (
            \ list -> List.map list ( \ listVal ->  op listVal val ) ) )


plusOp = \ left, right ->
     left + right

minusOp = \ left, right ->
     left - right

mulOp = \ left, right ->
     left * right

plusElemOp = \ left, right ->
     {right & value : right.value + left.value}


minusElemOp = \ left, right ->
     {right & value : right.value - left.value}

mulElemOp = \ left, right ->
     {right & value : right.value * left.value}


addXYLayerToCube =  \ cube, xyLayer  ->
    List.append cube xyLayer

solution = \ cube, cubeCharge, delta, edges -> 
    opDerivXCube cube  delta {plus : 0, minus : 0}  poissonDerivOp
    |> opCubes (opDerivYCube cube  delta {plus : 0, minus : 0}  poissonDerivOp) plusOp
    |> opCubes (opDerivZCube cube  delta {plus : 0, minus : 0}  poissonDerivOp ) plusOp
    |> opCubes cubeCharge minusOp
    |> opCube (  (power2 delta) / 6 ) mulOp
    

check = \ cube, delta, edges -> 
    opDerivXCube cube delta {plus : 0, minus : 0}  essenceDerivOp
    |> opCubes (opDerivYCube cube  delta {plus : 0, minus : 0}  essenceDerivOp) plusOp
    |> opCubes (opDerivZCube cube  delta {plus : 0, minus : 0}  essenceDerivOp ) plusOp

    

calculateSolution  =  \ cube, cubeCharge, delta, edges, cnt ->
    if cnt ==  0 then
        cube
    else
        solution  cube cubeCharge delta edges 
        |> calculateSolution cubeCharge delta edges ( cnt  - 1 )

sphere = \ x, y, z, r ->
    size = 2*r +1
    list = 
        increasedList size []
        |> shiftList -r  
    List.walk list  [] ( 
        \ stateX, idx -> List.walk list stateX (
            \ stateY, idy -> List.walk list stateY  (
                \ stateZ, idz ->
                    if ( Num.powInt idx 2 ) + ( Num.powInt idy 2 ) + ( Num.powInt idz 2 )  <= ( Num.powInt r 2 ) then  
                        List.append  stateZ  {x : Num.toNat(idx + x) ,y: Num.toNat(idy + y),z : Num.toNat(idz + z)}
                    else 
                        stateZ ))) 


cubid = \ x, y, z, xprim, yprim, zprim ->
    listX = increasedList (Num.abs (x - xprim)) []
    listY = increasedList (Num.abs (y - yprim)) []
    listZ = increasedList (Num.abs (z - zprim)) []

    List.walk listX  [] ( 
        \ stateX, idx -> List.walk listY stateX (
            \ stateY, idy -> List.walk listZ stateY  (
                \ stateZ, idz ->
                    checkSmaler =
                        ( \ left, right  -> 
                            if left < right then
                                 left
                            else 
                                right )

                    xShift = checkSmaler x  xprim
                    yShift = checkSmaler y  yprim
                    zShift = checkSmaler z  zprim
                    List.append  stateZ  {x : Num.toNat(idx + xShift) ,y: Num.toNat(idy + yShift),z : Num.toNat(idz + zShift)} )))


cilinder = \ x, y, z, r, height, axis  ->

    halfHeight =  Num.floor  ((Num.toF32 height)/2)
    halfR =  Num.floor  ((Num.toF32 r)/2)
    when axis is 
        X -> 
            cubid ( -halfHeight) (-halfR )  (-halfR ) ( halfHeight)  (halfR ) (halfR ) 
            |> List.walk  [] (
                    \ out ,point -> 
                        if ( Num.powInt point.y 2 ) + ( Num.powInt point.z 2 )  <= ( Num.powInt r 2 ) then
                            List.append  out  {x : Num.toNat(point.x + x) ,y: Num.toNat(point.y + y),z : Num.toNat(point.z + z)} 
                        else
                            out  )
        _ -> []

        


setShape = \ cube, shape, val ->
    List.walk shape cube  (
        \ cb, point -> 
            modifyFieldCube cb point.x point.y point.z val ) 
   
sliceCube = \ cube, slices ->
    when slices  is 
        { x : Idx xVal, y : All, z : All }  -> List.walk cube  [] ( \out ,sq  -> List.append out ( List.walk sq [] ( \ outList, list-> List.append  outList (Util.getFromList list xVal)  ) ) )   
        { x : All, y : Idx yVal, z : All }  -> List.walk cube [] ( \out ,sq -> List.append out ( Util.getListFromList sq  yVal ) )    
        { x : All, y : All, z : Idx zVal }  -> (Util.getListFromList cube zVal)    
        { x : Idx xVal, y : Idx yVal, z : All }  -> [List.walk cube  [] ( \out, sq -> List.append out (Util.getFromList (Util.getListFromList sq yVal) xVal  ) ) ] 
        { x : Idx xVal, y : All, z : Idx zVal  }  -> [List.walk (Util.getListFromList cube zVal) [] ( \list, out -> List.append out ( Util.getFromList list  xVal ) )]    
        { x : All, y : Idx yVal, z : Idx zVal  }  -> [(Util.getListFromList (Util.getListFromList cube zVal) yVal)]     
        { x : Idx xVal, y : Idx yVal, z : Idx zVal  }  -> [[ Util.getFromList (Util.getListFromList (Util.getListFromList cube zVal) yVal) xVal  ]]
        _ ->  []

addFrontAndBack = \ list, front , back  ->
    list
    |> List.prepend front
    |> List.append back

returnElem = \ list, index, defElem ->
    when List.get  list  index  is 
        Ok  elem -> elem
        Err OutOfBounds -> defElem

getFrontBack  = \  list  ->
    defElem = (Util.createNode   0 1 0)
    lastIdx = List.len list - 1
    { front : returnElem list 0 defElem, back : returnElem list lastIdx defElem }
    
lineMotion = \ orange, blueIn, force, prevEnds, cnt, out ->
    deltaX = 1
    deltaT = 0.2
    edges = {plus : (Util.createNode   0 1 0), minus : (Util.createNode   0 1 0)}
    
    blue = force  blueIn deltaT  cnt    
    
    if cnt == 0 then 
        out
    else
        orangePlusDeltaT =
            (opDerivXlist blue deltaX edges deriv1ElemOp )
            |> List.dropFirst
            |> List.map2 orange  (\ elemBlueMod, elemOrange  ->  { elemBlueMod & value : elemBlueMod.value - elemOrange.value  * elemOrange.omega }  )
            |> mulListElem deltaT
            |> List.map2 orange (\ elemBlue, elemOrange  ->  { elemBlue & value : elemBlue.value / elemOrange.param }  )
            |> List.map2 orange plusElemOp  


        frontBack = getFrontBack orangePlusDeltaT 
        frontBackBlue = getFrontBack blue 
        travelTimeFront = deltaX / (1/(Num.sqrt ( frontBack.front.param * frontBackBlue.front.param ) ))
        travelTimeBack = deltaX / (1/(Num.sqrt ( frontBack.back.param * frontBackBlue.back.param ) ))

        front = Util.createNode  (frontBack.front.value - ( frontBack.front.value - prevEnds.front.value) *(travelTimeFront/deltaT) )  frontBack.front.param  0
        back = Util.createNode ( frontBack.back.value - ( frontBack.back.value - prevEnds.back.value) *(travelTimeBack/deltaT) )   frontBack.back.param  0
        modOrange = addFrontAndBack orangePlusDeltaT front  back
        bluePlusDeltaT =
            (opDerivXlist modOrange deltaX edges deriv1ElemOp )
            |> List.dropFirst
            |> mulListElem deltaT
            |> List.map2 blue (\ elemOrange, elemBlue  -> { elemOrange & value : elemOrange.value / elemBlue.param }  )
            |> List.map2 blue plusElemOp

        lineMotion  orangePlusDeltaT bluePlusDeltaT force frontBack (cnt - 1) (  List.append  out blue)
      
      
xyVariationSim = \  zDirectionField, xDirectionField, yDirectionField, force, cnt, out  -> 
    deltaXY = 1
    deltaT = 0.2
    edges = {plus : (Util.createNode   0 1 0), minus : (Util.createNode   0 1 0)}
    
    zForced = force  zDirectionField deltaT  cnt  
    

    if cnt == 0 then 
        out
    else
        xDirectionPlusDeltaT =
            (opDerivYSq zForced deltaXY edges deriv1ElemOp )
            |> removeFirstSq
            |> sqElemOperation xDirectionField  (\ elemMod, elemSelf  ->  { elemMod & value : elemMod.value - elemSelf.value  * elemSelf.omega }  )
            |> mulSqElem deltaT
            |> sqElemOperation xDirectionField (\ elemMod, elemSelf  ->  { elemMod & value : elemMod.value / elemSelf.param }  )
            |> sqElemOperation xDirectionField plusElemOp 

        yDirectionFieldPlusDeltaT = 
            (opDerivXSq zForced deltaXY edges deriv1ElemOp )
            |> removeFirstSq
            |> sqElemOperation yDirectionField  (\ elemMod, elemSelf  ->  { elemMod & value : elemMod.value - elemSelf.value  * elemSelf.omega }  )
            |> mulSqElem deltaT
            |> sqElemOperation yDirectionField (\ elemMod, elemSelf  ->  { elemMod & value : elemMod.value / elemSelf.param }  )
            |> sqElemOperation yDirectionField plusElemOp 

        modXDir = 
            addFirstBackSq xDirectionPlusDeltaT edges.minus  edges.plus
            |> opDerivYSq  deltaXY edges deriv1ElemOp 
            |> removeFirstSq
            
        modYDir = 
            addFirstBackSq yDirectionFieldPlusDeltaT edges.minus  edges.plus
            |> opDerivXSq  deltaXY edges deriv1ElemOp 
            |> removeFirstSq
            
        zDirectionFieldDeltaT =
            sqElemOperation modXDir modYDir minusElemOp
            |> mulSqElem deltaT
            |> sqElemOperation zDirectionField (\ xyComp, zElem  -> { xyComp & value : xyComp.value / zElem.param }  )
            |> sqElemOperation zDirectionField plusElemOp

        xyVariationSim   xDirectionPlusDeltaT yDirectionFieldPlusDeltaT zDirectionFieldDeltaT force  (cnt - 1) {   out & zField  : addXYLayerToCube  out.zField zDirectionFieldDeltaT, xfield : addXYLayerToCube  out.xfield xDirectionPlusDeltaT ,  yfield :  addXYLayerToCube  out.yfield  yDirectionFieldPlusDeltaT }
      
xyzVariationSim = \   xDirectionField1, yDirectionField1, zDirectionField1, xDirectionField2, yDirectionField2, zDirectionField2,  force, cnt, out  -> 
    deltaXY = 1
    deltaT = 0.2
    edges = {plus : (Util.createNode   0 1 0), minus : (Util.createNode   0 1 0)}
    
    zForced = force  zDirectionField1 deltaT  cnt  
    
#opDerivXCube 
#opDerivYCube 
#opDerivZCube
    
    if cnt == 0 then 
        out
    else
        x1DirectionFieldPlusDeltaT =
            (opDerivYCube yDirectionField2 deltaXY edges deriv1ElemOp )
            |> removeFirstCube 
            |> cubeElemOperation (opDerivZCube  zDirectionField2 deltaXY edges deriv1ElemOp) minusElemOp
            |> cubeElemOperation xDirectionField1  (\ elemMod, elemSelf  ->  { elemMod & value : elemMod.value - elemSelf.value  * elemSelf.omega }  )
            |> mulCubeElem deltaT
            |> cubeElemOperation xDirectionField1 (\ elemMod, elemSelf  ->  { elemMod & value : elemMod.value / elemSelf.param }  )
            |> cubeElemOperation xDirectionField1 plusElemOp 

        y1DirectionFieldPlusDeltaT = 
            (opDerivZCube xDirectionField2 deltaXY edges deriv1ElemOp )
            |> removeFirstCube 
            |> cubeElemOperation (opDerivXCube  zDirectionField2 deltaXY edges deriv1ElemOp) minusElemOp
            |> cubeElemOperation yDirectionField1  (\ elemMod, elemSelf  ->   { elemMod & value : elemMod.value - elemSelf.value  * elemSelf.omega }  )
            |> mulCubeElem deltaT
            |> cubeElemOperation yDirectionField1 (\ elemMod, elemSelf  ->  { elemMod & value : elemMod.value / elemSelf.param }  )
            |> cubeElemOperation yDirectionField1 plusElemOp 

        z1DirectionFieldPlusDeltaT = 
            (opDerivXCube yDirectionField2 deltaXY edges deriv1ElemOp )
            |> removeFirstCube 
            |> cubeElemOperation (opDerivYCube  xDirectionField2 deltaXY edges deriv1ElemOp) minusElemOp
            |> cubeElemOperation zDirectionField1  (\ elemMod, elemSelf  ->   { elemMod & value : elemMod.value - elemSelf.value  * elemSelf.omega }  )
            |> mulCubeElem deltaT
            |> cubeElemOperation zDirectionField1 (\ elemMod, elemSelf  ->  { elemMod & value : elemMod.value / elemSelf.param }  )
            |> cubeElemOperation zDirectionField1 plusElemOp 

        modXDir = x1DirectionFieldPlusDeltaT
        #    addFirstBackSq x1DirectionFieldPlusDeltaT edges.minus  edges.plus
        #    |> opDerivYSq  deltaXY edges deriv1ElemOp 
        #    |> removeFirstSq
            
        modYDir = y1DirectionFieldPlusDeltaT
        #    addFirstBackSq y1DirectionFieldPlusDeltaT edges.minus  edges.plus
        #    |> opDerivXSq  deltaXY edges deriv1ElemOp 
        #    |> removeFirstSq
            
        x2DirectionFieldPlusDeltaT =
            (opDerivYCube yDirectionField1 deltaXY edges deriv1ElemOp )
            |> removeFirstCube 
            |> cubeElemOperation (opDerivZCube  zDirectionField1 deltaXY edges deriv1ElemOp) minusElemOp
            |> cubeElemOperation xDirectionField2  (\ elemMod, elemSelf  ->   { elemMod & value : elemMod.value - elemSelf.value  * elemSelf.omega }  )
            |> mulCubeElem deltaT
            |> cubeElemOperation xDirectionField2 (\ elemMod, elemSelf  ->  { elemMod & value : elemMod.value / elemSelf.param }  )
            |> cubeElemOperation xDirectionField2 plusElemOp 

        y2DirectionFieldPlusDeltaT = 
            (opDerivZCube xDirectionField1 deltaXY edges deriv1ElemOp )
            |> removeFirstCube 
            |> cubeElemOperation (opDerivXCube  zDirectionField1 deltaXY edges deriv1ElemOp) minusElemOp
            |> cubeElemOperation yDirectionField2  (\ elemMod, elemSelf  ->   { elemMod & value : elemMod.value - elemSelf.value  * elemSelf.omega }  )
            |> mulCubeElem deltaT
            |> cubeElemOperation yDirectionField2 (\ elemMod, elemSelf  ->  { elemMod & value : elemMod.value / elemSelf.param }  )
            |> cubeElemOperation yDirectionField2 plusElemOp 

        z2DirectionFieldPlusDeltaT = 
            (opDerivXCube yDirectionField1 deltaXY edges deriv1ElemOp )
            |> removeFirstCube 
            |> cubeElemOperation (opDerivYCube  xDirectionField1 deltaXY edges deriv1ElemOp) minusElemOp
            |> cubeElemOperation yDirectionField2  (\ elemMod, elemSelf  ->   { elemMod & value : elemMod.value - elemSelf.value  * elemSelf.omega }  )
            |> mulCubeElem deltaT
            |> cubeElemOperation yDirectionField2 (\ elemMod, elemSelf  ->  { elemMod & value : elemMod.value / elemSelf.param }  )
            |> cubeElemOperation yDirectionField2 plusElemOp 

        xyzVariationSim   x1DirectionFieldPlusDeltaT y1DirectionFieldPlusDeltaT z1DirectionFieldPlusDeltaT x2DirectionFieldPlusDeltaT y2DirectionFieldPlusDeltaT z2DirectionFieldPlusDeltaT force (cnt - 1)  out 
      