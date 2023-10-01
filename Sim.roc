interface Sim
    exposes [modifyFieldCube, modifyFieldSq, calculateSolution, makeCube, makeSquare, xyVariationSim2, xyzVariationSim2
            , sliceCube, check, sphere, setShape, lineMotion, xyVariationSim, xyzVariationSim, pmlIzeSq, pmlIzeCube, opDerivXCube, opDerivYCube,opDerivZCube, cubeElemOperation,  minusElemOp ]
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



getFeldSq  = \ sq, x, y, def ->
    Util.getListFromList sq  0
    |> Util.getFromList  x  def

    
modifyFieldSq  = \ square, x, y, val ->
    Util.getListFromList square y
    |> List.replace x val 
    |> (\ updated -> (List.replace square y updated.list).list  )

removeFirstSqY = \ square ->
    square
    |> List.dropFirst

removeFirstSqX = \ square ->
    List.map square List.dropFirst


removeFirstCubeX = \ cube ->
    List.map cube  removeFirstSqX

removeFirstCubeY = \ cube ->
    List.map cube List.dropFirst 

removeFirstCubeZ = \ cube ->
    List.dropFirst cube

mulSqElem = \ square, mod ->
    List.map square  (\ list -> mulListElem list mod )

mulCubeElem = \ cube, mod ->
    List.map cube  (\ sq -> mulSqElem sq mod )

sqElemOperation = \ squareL, squareR, op ->
    List.map2 squareL squareR (\ listL, listR  -> List.map2 listL listR  op )


cubeElemOperation = \ cubeL, cubeR, op ->
    List.map2 cubeL cubeR  (\ sqL, sqR  -> sqElemOperation sqL sqR  op )

addFirstBackSqY = \ square, first, back ->
    len = List.len (Util.getListFromList  square 0)
    square
    |> List.prepend (List.repeat first len)
    |> List.append (List.repeat back len)

addFirstBackSqX = \ square, front, back ->
    List.map square  (\ list -> addFrontAndBack list front  back  )

getCubeSizeX = \ cube ->
    (Util.getListFromList cube 0)
    |> Util.getListFromList 0
    |> List.len 
    
getCubeSizeY = \ cube ->
    List.len (Util.getListFromList cube 0)
    
getCubeSizeZ = \ cube ->
    List.len cube

getSqSizeX = \ sq ->
    List.len (Util.getListFromList sq 0)
    
    
getSqSizeY = \ sq ->
    List.len sq 


addFirstBackCubeX = \ cube, front, back ->
    List.map cube  (\ sq -> addFirstBackSqX sq front  back  )

addFirstBackCubeY = \ cube, front, back ->    
    List.map cube  (\ sq -> addFirstBackSqY sq  front  back )
  
addFirstBackCubeZ = \ cube, front, back ->
    cube  
    |> List.prepend (makeSquare   (getCubeSizeX  cube) (getCubeSizeY  cube)  front )
    |> List.append (makeSquare   (getCubeSizeX  cube) (getCubeSizeY  cube)  back )


getFeldCube  = \ cube, x, y, z, def ->
    Util.getListFromList cube z
    |> Util.getListFromList y
    |> Util.getFromList  x  def

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
    listYMinus = 
        lists 
        |> List.prepend (List.repeat edges.plus len)
        |> List.dropLast
    listYPlus = 
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
          
    listZMinus = 
        cube
        |> List.prepend (makeSquare lenX lenY edges.plus  )
        |> List.dropLast 
    listZPlus =
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
     {right & value : left.value - right.value}

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
        { x : Idx xVal, y : All, z : All }  -> List.walk cube  [] ( \out ,sq  -> List.append out ( List.walk sq [] ( \ outList, list-> List.append  outList (Util.getFromList list xVal  0.0f32)  ) ) )   
        { x : All, y : Idx yVal, z : All }  -> List.walk cube [] ( \out ,sq -> List.append out ( Util.getListFromList sq  yVal ) )    
        { x : All, y : All, z : Idx zVal }  -> (Util.getListFromList cube zVal)    
        { x : Idx xVal, y : Idx yVal, z : All }  -> [List.walk cube  [] ( \out, sq -> List.append out (Util.getFromList (Util.getListFromList sq yVal) xVal  0.0f32) ) ] 
        { x : Idx xVal, y : All, z : Idx zVal  }  -> [List.walk (Util.getListFromList cube zVal) [] ( \list, out -> List.append out ( Util.getFromList list  xVal 0.0f32) )]    
        { x : All, y : Idx yVal, z : Idx zVal  }  -> [(Util.getListFromList (Util.getListFromList cube zVal) yVal)]     
        { x : Idx xVal, y : Idx yVal, z : Idx zVal  }  -> [[ Util.getFromList (Util.getListFromList (Util.getListFromList cube zVal) yVal) xVal  0.0f32]]
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
    
lineMotion = \ params, orange, blueIn, force, prevEnds, cnt, out ->
    deltaX = params.deltaSpace
    deltaT = params.deltaT
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

        lineMotion params orangePlusDeltaT bluePlusDeltaT force frontBack (cnt - 1) (  List.append  out blue)
      
getRele = \ elem ->
    elem.value
      
calculateFieldModif = \ elem, deltaT  ->
    (2*elem.param - deltaT * elem.omega)/(2*elem.param + deltaT * elem.omega)
    
calculateDivModif = \ elem, deltaT  ->
    (2*deltaT)/(2*elem.param + deltaT * elem.omega)
          
xyVariationSim = \ params, xDirectionField1, yDirectionField1, zDirectionField2, force, cnt, out  -> 
    deltaXY = params.deltaSpace
    deltaT = params.deltaT
    edges = {plus : (Util.createNode   0 1 0), minus : (Util.createNode   0 1 0)}
    
    zForced = force  zDirectionField2 deltaT  cnt  

    if cnt == 0 then 
        out
    else
        xDirectionField1PlusDeltaT =
            addFirstBackSqY zForced edges.minus  edges.plus
            |> opDerivYSq  deltaXY edges deriv1ElemOp 
            |> removeFirstSqY
            |> sqElemOperation xDirectionField1  (\ elemMod, elemSelf  ->  { elemSelf & value : ( calculateFieldModif elemSelf deltaT )*elemSelf.value + (calculateDivModif elemSelf deltaT ) * elemMod.value }  )
        test = addFirstBackSqY zForced edges.minus  edges.plus
            |> opDerivYSq  deltaXY edges deriv1ElemOp 
            |> removeFirstSqY
            |> sqElemOperation xDirectionField1  (\ elemMod, elemSelf  ->  { elemSelf & value : ( calculateDivModif elemSelf deltaT ) }  )
        yDirectionField1PlusDeltaT = 
            addFirstBackSqX zForced edges.minus  edges.plus
            |> opDerivXSq  deltaXY edges deriv1ElemOp 
            |> removeFirstSqX
            |> sqElemOperation yDirectionField1  (\ elemMod, elemSelf  ->  { elemSelf & value : ( calculateFieldModif elemSelf deltaT )*elemSelf.value - (calculateDivModif elemSelf deltaT ) * elemMod.value }  )

        modXDir = 
            opDerivYSq xDirectionField1PlusDeltaT deltaXY edges deriv1ElemOp 
            |> removeFirstSqY
            
        modYDir = 
            opDerivXSq yDirectionField1PlusDeltaT deltaXY edges deriv1ElemOp 
            |> removeFirstSqX
            
        zDirectionField2PlusDeltaT =
            sqElemOperation modXDir modYDir minusElemOp
            |> sqElemOperation zDirectionField2 (\ elemMod, elemSelf  ->  { elemSelf & value : ( calculateFieldModif elemSelf deltaT )*elemSelf.value + (calculateDivModif elemSelf deltaT ) * elemMod.value }  )

        xyVariationSim  params xDirectionField1PlusDeltaT yDirectionField1PlusDeltaT zDirectionField2PlusDeltaT  force  (cnt - 1) {   out & zField  : addXYLayerToCube  out.zField zDirectionField2PlusDeltaT, xField : addXYLayerToCube  out.xField test ,  yField :  addXYLayerToCube  out.yField  yDirectionField1PlusDeltaT }
      

      
xyzVariationSim = \ params, xDirectionField1, yDirectionField1, zDirectionField1, xDirectionField2, yDirectionField2, zDirectionField2,  force, cnt, out  -> 
    deltaXY = params.deltaSpace
    deltaT = params.deltaT
    edges = {minus : (Util.createNode   0 1 0), plus : (Util.createNode   0 1 0)}
    
    modified = force  xDirectionField1 yDirectionField1 zDirectionField1 xDirectionField2 yDirectionField2 zDirectionField2 deltaT  cnt  
    
    if cnt == 0 then 
        out
    else
        xDirectionField1PlusDeltaT =
            (opDerivYCube (addFirstBackCubeY modified.zField2 edges.minus edges.plus) deltaXY edges deriv1ElemOp )
            |> removeFirstCubeY 
            |> cubeElemOperation (removeFirstCubeZ (opDerivZCube  (addFirstBackCubeZ modified.yField2 edges.minus edges.plus) deltaXY edges deriv1ElemOp)) minusElemOp
            |> cubeElemOperation modified.xField1  (\ elemMod, elemSelf  ->  { elemSelf & value : ( calculateFieldModif elemSelf deltaT )*elemSelf.value + (calculateDivModif elemSelf deltaT ) * elemMod.value }  )
        
        test = 
            (opDerivYCube (addFirstBackCubeY modified.zField2 edges.minus edges.plus) deltaXY edges deriv1ElemOp )
            |> removeFirstCubeY 
            |> cubeElemOperation (removeFirstCubeZ (opDerivZCube  (addFirstBackCubeZ modified.yField2 edges.minus edges.plus) deltaXY edges deriv1ElemOp)) minusElemOp
            
        yDirectionField1PlusDeltaT = 
            (opDerivZCube  (addFirstBackCubeZ modified.xField2 edges.minus edges.plus) deltaXY edges deriv1ElemOp)
            |> removeFirstCubeZ
            |> cubeElemOperation (removeFirstCubeX (opDerivXCube  (addFirstBackCubeX modified.zField2 edges.minus edges.plus) deltaXY edges deriv1ElemOp)) minusElemOp
            |> cubeElemOperation modified.yField1  (\ elemMod, elemSelf  ->  { elemSelf & value : ( calculateFieldModif elemSelf deltaT )*elemSelf.value + (calculateDivModif elemSelf deltaT ) * elemMod.value }  )

        zDirectionField1PlusDeltaT = 
            (opDerivXCube (addFirstBackCubeX modified.yField2 edges.minus edges.plus) deltaXY edges deriv1ElemOp )
            |> removeFirstCubeX
            |> cubeElemOperation (removeFirstCubeY (opDerivYCube   (addFirstBackCubeY modified.xField2 edges.minus edges.plus) deltaXY edges deriv1ElemOp)) minusElemOp
            |> cubeElemOperation modified.zField1 (\ elemMod, elemSelf  ->  { elemSelf & value : ( calculateFieldModif elemSelf deltaT )*elemSelf.value + (calculateDivModif elemSelf deltaT ) * elemMod.value }  )

        xDirectionField2PlusDeltaT =
            opDerivZCube yDirectionField1PlusDeltaT  deltaXY edges deriv1ElemOp 
            |> removeFirstCubeZ 
            |> cubeElemOperation (removeFirstCubeY(opDerivYCube  zDirectionField1PlusDeltaT  deltaXY edges deriv1ElemOp)) minusElemOp
            |> cubeElemOperation modified.xField2  (\ elemMod, elemSelf  ->  { elemSelf & value : ( calculateFieldModif elemSelf deltaT )*elemSelf.value + (calculateDivModif elemSelf deltaT ) * elemMod.value }  )

        yDirectionField2PlusDeltaT = 
            opDerivXCube zDirectionField1PlusDeltaT  deltaXY edges deriv1ElemOp 
            |> removeFirstCubeX 
            |> cubeElemOperation (removeFirstCubeZ (opDerivZCube  xDirectionField1PlusDeltaT deltaXY edges deriv1ElemOp)) minusElemOp
            |> cubeElemOperation modified.yField2  (\ elemMod, elemSelf  ->  { elemSelf & value : ( calculateFieldModif elemSelf deltaT )*elemSelf.value + (calculateDivModif elemSelf deltaT ) * elemMod.value }  )

        zDirectionField2PlusDeltaT = 
            opDerivYCube  xDirectionField1PlusDeltaT  deltaXY edges deriv1ElemOp
            |> removeFirstCubeY 
            |> cubeElemOperation (removeFirstCubeX(opDerivXCube  yDirectionField1PlusDeltaT deltaXY edges deriv1ElemOp)) minusElemOp
            |> cubeElemOperation modified.zField2  (\ elemMod, elemSelf  ->  { elemSelf & value : ( calculateFieldModif elemSelf deltaT )*elemSelf.value + (calculateDivModif elemSelf deltaT ) * elemMod.value }  )
        #xyzVariationSim params xDirectionField1PlusDeltaT yDirectionField1PlusDeltaT zDirectionField1PlusDeltaT xDirectionField2PlusDeltaT yDirectionField2PlusDeltaT zDirectionField2PlusDeltaT force (cnt - 1)  { out & xField1 : List.append out.xField1  test,  yField1 : List.append out.yField1  yDirectionField1PlusDeltaT, zField1 : List.append out.zField1  zDirectionField1PlusDeltaT, xField2 : List.append out.xField2  xDirectionField2PlusDeltaT,  yField2 : List.append out.yField2  yDirectionField2PlusDeltaT, zField2  : List.append out.zField2 zDirectionField2PlusDeltaT }  
        xyzVariationSim params xDirectionField1PlusDeltaT yDirectionField1PlusDeltaT zDirectionField1PlusDeltaT xDirectionField2PlusDeltaT yDirectionField2PlusDeltaT zDirectionField2PlusDeltaT force (cnt - 1)  { out & xField1 : List.append out.xField1  xDirectionField1PlusDeltaT,  yField1 : List.append out.yField1  yDirectionField1PlusDeltaT, zField1 : List.append out.zField1  zDirectionField1PlusDeltaT, xField2 : List.append out.xField2  xDirectionField2PlusDeltaT,  yField2 : List.append out.yField2  yDirectionField2PlusDeltaT, zField2  : List.append out.zField2 zDirectionField2PlusDeltaT } 
      
      
calculateFieldModifAni = \ param, omega, deltaT  ->
    (2*param - deltaT * omega)/(2*param + deltaT * omega)
    
calculateDivModifAni = \ param, omega, deltaT  ->
    (2*deltaT)/(2*param + deltaT * omega)

auxDalculateSumModifAni = \ param, omegaNum, omegaDen, deltaT  ->
    (2*param + deltaT * omegaNum)/((2*param + deltaT * omegaDen)/param)

auxDalculateSubModifAni = \ param, omegaNum, omegaDen, deltaT  ->
    (2*param - deltaT * omegaNum)/((2*param + deltaT * omegaDen)/param)
    
xyVariationSim2Internal = \ params, xDirectionAuxField1, yDirectionAuxField1, xDirectionField1, yDirectionField1, zDirectionAuxField2, zDirectionField2, force, cnt, out  -> 
    deltaXY = params.deltaSpace
    deltaT = params.deltaT
    edges = {plus : (Util.createNodeAni   0 1 0 0 0), minus : (Util.createNodeAni   0 1 0 0 0)}
    
    zForced = force  zDirectionField2 deltaT  cnt  

    if cnt == 0 then 
        out
    else    
        xDirectionAuxField1PlusDeltaT =
            (opDerivYSq (addFirstBackSqY zForced edges.minus edges.plus) deltaXY edges deriv1ElemOp )
            |> removeFirstSqY 
            |> sqElemOperation xDirectionAuxField1  (\ elemMod, elemSelf  ->  { elemSelf & value : ( calculateFieldModifAni elemSelf.param elemSelf.omega.y deltaT )*elemSelf.value +(calculateDivModifAni elemSelf.param elemSelf.omega.y deltaT ) * elemMod.value }  )

        yDirectionAuxField1PlusDeltaT = 
            (opDerivXSq  (addFirstBackSqX zForced edges.minus edges.plus) deltaXY edges deriv1ElemOp) 
            |> removeFirstSqX
            |> sqElemOperation yDirectionAuxField1  (\ elemMod, elemSelf  ->  { elemSelf & value : ( calculateFieldModifAni elemSelf.param elemSelf.omega.z deltaT )*elemSelf.value - (calculateDivModifAni elemSelf.param elemSelf.omega.z deltaT ) * elemMod.value }  )

        xDirectionField1PlusDeltaT =
            sqElemOperation xDirectionAuxField1PlusDeltaT xDirectionAuxField1 
                (\ auxPlusDelta, aux -> { aux & value : (auxDalculateSumModifAni aux.param aux.omega.x aux.omega.z deltaT )* auxPlusDelta.value - (auxDalculateSubModifAni  aux.param aux.omega.x aux.omega.z deltaT ) * aux.value })
            |> sqElemOperation xDirectionField1 (\ aux, field -> { field & value : (calculateFieldModifAni field.param field.omega.z deltaT)* field.value + aux.value })

        yDirectionField1PlusDeltaT = 
            sqElemOperation yDirectionAuxField1PlusDeltaT yDirectionAuxField1
                (\ auxPlusDelta, aux -> { aux & value : (auxDalculateSumModifAni aux.param aux.omega.y aux.omega.x deltaT ) * auxPlusDelta.value - (auxDalculateSubModifAni  aux.param aux.omega.y aux.omega.x deltaT ) * aux.value })
            |> sqElemOperation yDirectionField1  (\ aux, field -> { field & value : (calculateFieldModifAni field.param field.omega.x deltaT)* field.value + aux.value })

        zDirectionAuxField2PlusDeltaT = 
            (opDerivYSq   xDirectionField1PlusDeltaT  deltaXY edges deriv1ElemOp)
            |> removeFirstSqY
            |> sqElemOperation (removeFirstSqX (opDerivXSq yDirectionField1PlusDeltaT deltaXY edges deriv1ElemOp )) minusElemOp
            |> sqElemOperation zDirectionAuxField2 (\ elemMod, elemSelf  ->  { elemSelf & value : ( calculateFieldModifAni elemSelf.param elemSelf.omega.x deltaT )*elemSelf.value + (calculateDivModifAni elemSelf.param elemSelf.omega.x deltaT ) * elemMod.value }  )
        #dbg ( Sim.makeStringSq xDirectionAuxField1PlusDeltaT getRelevantt  "\n" )
        #dbg ( Sim.makeStringSq yDirectionAuxField1PlusDeltaT getRelevantt  "\n" )
               
        zDirectionField2PlusDeltaT =
            sqElemOperation zDirectionAuxField2PlusDeltaT zDirectionAuxField2
                (\ auxPlusDelta, aux -> { aux & value : (auxDalculateSumModifAni aux.param aux.omega.z aux.omega.y deltaT ) * auxPlusDelta.value - (auxDalculateSubModifAni  aux.param aux.omega.z aux.omega.y deltaT )* aux.value })
            |> sqElemOperation zForced (\ aux, field -> { field & value : (calculateFieldModifAni field.param field.omega.y deltaT)* field.value + aux.value } )

        xyVariationSim2Internal  params xDirectionAuxField1PlusDeltaT yDirectionAuxField1PlusDeltaT  xDirectionField1PlusDeltaT yDirectionField1PlusDeltaT zDirectionAuxField2PlusDeltaT zDirectionField2PlusDeltaT  force  (cnt - 1) {   out & zField  : addXYLayerToCube  out.zField zDirectionField2PlusDeltaT, xField : addXYLayerToCube  out.xField xDirectionField1PlusDeltaT ,  yField :  addXYLayerToCube  out.yField  yDirectionField1PlusDeltaT }
      
xyVariationSim2 = \ params, xDirectionField1, yDirectionField1, zDirectionField2, force, cnt, out ->
    xyVariationSim2Internal params xDirectionField1 yDirectionField1 xDirectionField1 yDirectionField1 zDirectionField2 zDirectionField2 force cnt out
    
xyzVariationSim2Internal = \ params, xDirectionAuxField1, yDirectionAuxField1, zDirectionAuxField1, xDirectionField1, yDirectionField1, zDirectionField1, xDirectionAuxField2, yDirectionAuxField2, zDirectionAuxField2, xDirectionField2, yDirectionField2, zDirectionField2,  force, cnt, out  -> 
    deltaXY = params.deltaSpace
    deltaT = params.deltaT
    edges = {minus : (Util.createNodeAni   0 1 0 0 0), plus : (Util.createNodeAni   0 1 0 0 0)}
    
    modified = force  xDirectionField1 yDirectionField1 zDirectionField1 xDirectionField2 yDirectionField2 zDirectionField2 deltaT  cnt  
    
    if cnt == 0 then 
        out
    else
        xDirectionAuxField1PlusDeltaT =
            (opDerivYCube (addFirstBackCubeY modified.zField2 edges.minus edges.plus) deltaXY edges deriv1ElemOp )
            |> removeFirstCubeY 
            |> cubeElemOperation (removeFirstCubeZ (opDerivZCube  (addFirstBackCubeZ modified.yField2 edges.minus edges.plus) deltaXY edges deriv1ElemOp)) minusElemOp
            |> cubeElemOperation xDirectionAuxField1  (\ elemMod, elemSelf  ->  { elemSelf & value : ( calculateFieldModifAni elemSelf.param elemSelf.omega.y deltaT )*elemSelf.value + (calculateDivModifAni elemSelf.param elemSelf.omega.y deltaT ) * elemMod.value }  )

        yDirectionAuxField1PlusDeltaT = 
            (opDerivZCube  (addFirstBackCubeZ modified.xField2 edges.minus edges.plus) deltaXY edges deriv1ElemOp)
            |> removeFirstCubeZ
            |> cubeElemOperation (removeFirstCubeX (opDerivXCube  (addFirstBackCubeX modified.zField2 edges.minus edges.plus) deltaXY edges deriv1ElemOp)) minusElemOp
            |> cubeElemOperation yDirectionAuxField1  (\ elemMod, elemSelf  ->  { elemSelf & value : ( calculateFieldModifAni elemSelf.param elemSelf.omega.z deltaT )*elemSelf.value + (calculateDivModifAni elemSelf.param elemSelf.omega.z deltaT ) * elemMod.value }  )

        zDirectionAuxField1PlusDeltaT = 
            (opDerivXCube (addFirstBackCubeX modified.yField2 edges.minus edges.plus) deltaXY edges deriv1ElemOp )
            |> removeFirstCubeX
            |> cubeElemOperation (removeFirstCubeY (opDerivYCube   (addFirstBackCubeY modified.xField2 edges.minus edges.plus) deltaXY edges deriv1ElemOp)) minusElemOp
            |> cubeElemOperation zDirectionAuxField1 (\ elemMod, elemSelf  ->  { elemSelf & value : ( calculateFieldModifAni elemSelf.param elemSelf.omega.x deltaT )*elemSelf.value + (calculateDivModifAni elemSelf.param elemSelf.omega.x deltaT ) * elemMod.value }  )

        xDirectionField1PlusDeltaT =
            cubeElemOperation xDirectionAuxField1PlusDeltaT xDirectionAuxField1 
                (\ auxPlusDelta, aux -> { aux & value : (auxDalculateSumModifAni aux.param aux.omega.x aux.omega.z deltaT ) * auxPlusDelta.value - (auxDalculateSubModifAni  aux.param aux.omega.x aux.omega.z deltaT ) * aux.value })
            |> cubeElemOperation modified.xField1  (\ aux, field -> { field & value : (calculateFieldModifAni field.param field.omega.z deltaT)* field.value  + aux.value })

        yDirectionField1PlusDeltaT = 
            cubeElemOperation yDirectionAuxField1PlusDeltaT yDirectionAuxField1
                (\ auxPlusDelta, aux -> { aux & value : (auxDalculateSumModifAni aux.param aux.omega.y aux.omega.x deltaT ) * auxPlusDelta.value - (auxDalculateSubModifAni  aux.param aux.omega.y aux.omega.x deltaT ) * aux.value})
            |> cubeElemOperation modified.yField1 (\ aux, field -> { field & value : (calculateFieldModifAni field.param field.omega.x deltaT)* field.value  + aux.value })

        zDirectionField1PlusDeltaT = 
            cubeElemOperation zDirectionAuxField1PlusDeltaT zDirectionAuxField1
                (\ auxPlusDelta, aux -> { aux & value : (auxDalculateSumModifAni aux.param aux.omega.z aux.omega.y deltaT ) * auxPlusDelta.value - (auxDalculateSubModifAni  aux.param aux.omega.z aux.omega.y deltaT ) * aux.value})
            |> cubeElemOperation modified.zField1 (\ aux, field -> { field & value : (calculateFieldModifAni field.param field.omega.y deltaT)* field.value  + aux.value })

        xDirectionAuxField2PlusDeltaT =
            (opDerivZCube yDirectionField1PlusDeltaT deltaXY edges deriv1ElemOp)
            |> removeFirstCubeZ 
            |> cubeElemOperation (removeFirstCubeY (opDerivYCube zDirectionField1PlusDeltaT deltaXY edges deriv1ElemOp)) minusElemOp
            |> cubeElemOperation xDirectionAuxField2 (\ elemMod, elemSelf  ->  { elemSelf & value : ( calculateFieldModifAni elemSelf.param elemSelf.omega.y deltaT )*elemSelf.value + (calculateDivModifAni elemSelf.param elemSelf.omega.y deltaT ) * elemMod.value }  )

        yDirectionAuxField2PlusDeltaT = 
            (opDerivXCube zDirectionField1PlusDeltaT deltaXY edges deriv1ElemOp)
            |> removeFirstCubeX
            |> cubeElemOperation (removeFirstCubeZ (opDerivZCube xDirectionField1PlusDeltaT deltaXY edges deriv1ElemOp)) minusElemOp
            |> cubeElemOperation yDirectionAuxField2 (\ elemMod, elemSelf  ->  { elemSelf & value : ( calculateFieldModifAni elemSelf.param elemSelf.omega.z deltaT )*elemSelf.value + (calculateDivModifAni elemSelf.param elemSelf.omega.z deltaT ) * elemMod.value }  )

        zDirectionAuxField2PlusDeltaT = 
            (opDerivYCube xDirectionField1PlusDeltaT deltaXY edges deriv1ElemOp)
            |> removeFirstCubeY
            |> cubeElemOperation (removeFirstCubeX (opDerivXCube yDirectionField1PlusDeltaT deltaXY edges deriv1ElemOp)) minusElemOp
            |> cubeElemOperation zDirectionAuxField2 (\ elemMod, elemSelf  ->  { elemSelf & value : ( calculateFieldModifAni elemSelf.param elemSelf.omega.x deltaT )*elemSelf.value + (calculateDivModifAni elemSelf.param elemSelf.omega.x deltaT ) * elemMod.value }  )

        xDirectionField2PlusDeltaT =
            cubeElemOperation xDirectionAuxField2PlusDeltaT xDirectionAuxField2 
                (\ auxPlusDelta, aux -> { aux & value : (auxDalculateSumModifAni aux.param aux.omega.x aux.omega.z deltaT ) * auxPlusDelta.value - (auxDalculateSubModifAni  aux.param aux.omega.x aux.omega.z deltaT ) * aux.value })
            |> cubeElemOperation modified.xField2 (\ aux, field -> { field & value : (calculateFieldModifAni field.param field.omega.z deltaT)* field.value  + aux.value })

        yDirectionField2PlusDeltaT = 
            cubeElemOperation yDirectionAuxField2PlusDeltaT yDirectionAuxField2
                (\ auxPlusDelta, aux -> { aux & value : (auxDalculateSumModifAni aux.param aux.omega.y aux.omega.x deltaT ) * auxPlusDelta.value - (auxDalculateSubModifAni  aux.param aux.omega.y aux.omega.x deltaT ) * aux.value })
            |> cubeElemOperation modified.yField2 (\ aux, field -> { field & value : (calculateFieldModifAni field.param field.omega.x deltaT)* field.value  + aux.value })

        zDirectionField2PlusDeltaT = 
            cubeElemOperation zDirectionAuxField2PlusDeltaT zDirectionAuxField2
                (\ auxPlusDelta, aux -> { aux & value : (auxDalculateSumModifAni aux.param aux.omega.z aux.omega.y deltaT ) * auxPlusDelta.value - (auxDalculateSubModifAni  aux.param aux.omega.z aux.omega.y deltaT ) * aux.value })
            |> cubeElemOperation modified.zField2 (\ aux, field -> { field & value : (calculateFieldModifAni field.param field.omega.y deltaT)* field.value  + aux.value })


        test =
            zDirectionAuxField2PlusDeltaT

        xyzVariationSim2Internal params xDirectionAuxField1PlusDeltaT yDirectionAuxField1PlusDeltaT zDirectionAuxField1PlusDeltaT xDirectionField1PlusDeltaT yDirectionField1PlusDeltaT zDirectionField1PlusDeltaT   xDirectionAuxField2PlusDeltaT yDirectionAuxField2PlusDeltaT zDirectionAuxField2PlusDeltaT xDirectionField2PlusDeltaT yDirectionField2PlusDeltaT zDirectionField2PlusDeltaT force (cnt - 1)  { out & xField1 : List.append out.xField1  xDirectionField1PlusDeltaT,  yField1 : List.append out.yField1  yDirectionField1PlusDeltaT, zField1 : List.append out.zField1  zDirectionField1PlusDeltaT, xField2 : List.append out.xField2  xDirectionField2PlusDeltaT,  yField2 : List.append out.yField2  yDirectionField2PlusDeltaT, zField2  : List.append out.zField2 zDirectionField2PlusDeltaT } 
      
      
xyzVariationSim2 = \ params, xDirectionField1, yDirectionField1, zDirectionField1, xDirectionField2, yDirectionField2, zDirectionField2,  force, cnt, out  -> 
    xyzVariationSim2Internal params xDirectionField1 yDirectionField1 zDirectionField1 xDirectionField1 yDirectionField1 zDirectionField1 xDirectionField2 yDirectionField2 zDirectionField2 xDirectionField2 yDirectionField2 zDirectionField2  force cnt out 

iterate = \ arg, cnt, exe  -> 
    if cnt == 0 then
        (exe arg cnt) 
    else
        iterate (exe arg cnt) (cnt - 1) exe 



shapingFunction = \ x, layers, mi, epsi ->
    m = 2.5f32
    r0 = 0.001f32
    d = Num.toF32 layers
    omegaMax = (m + 1 ) *(Num.log (Num.toF32 r0) ) /(2* d * ( Num.sqrt (( Num.toF32 mi) / Num.toF32 epsi )) )
    ( Num.pow ( (d- (Num.toF32 x) ) / d) m  )* (-omegaMax)
        
pmlIzeSq  = \ sq, layers , mi , epsi  ->
 
    sizeX = getSqSizeX sq - 1
    sizeY = getSqSizeY sq - 1    

    xEdgesAlteredSq = iterate sq sizeY ( \ sq1, cntY ->
            iterate sq1 layers ( \ sq2, cntX-> 
                modifFront = getFeldSq  sq2 cntX  cntY (Util.createNodeAni   0 1 0 0 0)
                modifBack = getFeldSq  sq2 (sizeX - cntX) cntY (Util.createNodeAni   0 1 0 0 0)
                modifyFieldSq sq2 cntX  cntY  {modifFront &  omega : { x:(shapingFunction cntX layers mi epsi), y :  modifFront.omega.y, z: modifFront.omega.z } }     
                |> modifyFieldSq  (sizeX - cntX)  cntY  {modifBack &  omega : { x:(shapingFunction cntX layers mi epsi), y :  modifBack.omega.y, z: modifBack.omega.z } }  )  )

    iterate xEdgesAlteredSq sizeX ( \ sq1, cntX ->
            iterate sq1 layers ( \ sq2, cntY-> 
                modifFront = getFeldSq  sq2 cntX  cntY (Util.createNodeAni   0 1 0 0 0)
                modifBack = getFeldSq  sq2 cntX (sizeY - cntY) (Util.createNodeAni   0 1 0 0 0)
                modifyFieldSq sq2 cntX  cntY  {modifFront &  omega : { x:modifFront.omega.x, y : (shapingFunction cntY layers mi epsi), z: modifFront.omega.z } }     
                |> modifyFieldSq  cntX  (sizeY - cntY)  {modifBack &  omega : { x:modifBack.omega.x, y : (shapingFunction cntY layers mi epsi), z: modifBack.omega.z } }  )  )

pmlIzeCube  = \ cube, layers, mi, epsi   ->

    sizeX = getCubeSizeX cube - 1
    sizeY = getCubeSizeY cube - 1
    sizeZ = getCubeSizeZ cube - 1

    xEdgesAlteredCb = iterate cube sizeZ ( \ cube1, cntZ -> 
        iterate cube1 sizeY ( \ cube2, cntY ->
            iterate cube2 layers ( \ cube3, cntX-> 
                modifFront = getFeldCube  cube3 cntX  cntY cntZ (Util.createNodeAni   0 1 0 0 0)
                modifBack = getFeldCube  cube3  (sizeX - cntX)  cntY cntZ (Util.createNodeAni   0 1 0 0 0)
                modifyFieldCube cube3 cntX  cntY cntZ {modifFront &  omega : { x:(shapingFunction cntX layers mi epsi), y :  modifFront.omega.y, z: modifFront.omega.z } }     
                |> modifyFieldCube  (sizeX - cntX)  cntY cntZ {modifBack &  omega : { x:(shapingFunction cntX layers mi epsi), y :  modifBack.omega.y, z: modifBack.omega.z } }  )  ))
    yEdgesAlteredCb = iterate xEdgesAlteredCb sizeZ ( \ cube1, cntZ -> 
        iterate cube1 sizeX ( \ cube2, cntX ->
            iterate cube2 layers ( \ cube3, cntY-> 
                modifFront = getFeldCube  cube3 cntX  cntY cntZ (Util.createNodeAni   0 1 0 0 0)
                modifBack = getFeldCube  cube3  cntX  (sizeY - cntY) cntZ (Util.createNodeAni   0 1 0 0 0)
                modifyFieldCube cube3 cntX  cntY cntZ {modifFront &  omega : { x: modifFront.omega.x, y :(shapingFunction cntY layers mi epsi), z: modifFront.omega.z   } }     
                |> modifyFieldCube  cntX (sizeY - cntY) cntZ {modifBack &  omega : {  x: modifBack.omega.x, y : (shapingFunction cntY layers mi epsi), z: modifBack.omega.z  } }  )  ))

    iterate yEdgesAlteredCb sizeX ( \ cube1, cntX -> 
        iterate cube1 sizeY ( \ cube2, cntY ->
            iterate cube2 layers ( \ cube3, cntZ-> 
                modifFront = getFeldCube  cube3 cntX  cntY cntZ  (Util.createNodeAni   0 1 0 0 0)
                modifBack = getFeldCube  cube3  cntX  cntY (sizeZ - cntZ)  (Util.createNodeAni   0 1 0 0 0)
                modifyFieldCube cube3 cntX  cntY cntZ {modifFront &  omega : {  x: modifFront.omega.x, y: modifFront.omega.y,  z : (shapingFunction cntZ layers mi epsi)} }     
                |> modifyFieldCube  cntX cntY (sizeZ - cntZ) {modifBack &  omega : { x: modifBack.omega.x, y: modifBack.omega.y,  z : (shapingFunction cntZ layers mi epsi)} }  )  ))


