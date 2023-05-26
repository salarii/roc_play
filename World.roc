interface World
    exposes [chengeParam,runWorld,createWorld ]
    imports []

t = 0.0002


createElement = \ mass, size, velocity,young,damping -> 
    {mass : mass, young : young, size : size, velocity : velocity, damping : damping}

empty = createElement 40 0.0 0.0 200000 0.0

validate = \ result ->
    when result is
        Ok res -> res
        Err _ -> createElement 0.0 0.0 0.0 0.0 0.0

calcforce =  \elem -> 
    -elem.size*elem.young

acceleration = \ elemLeft, elem, elemRight  ->
    ((calcforce elemLeft) - (calcforce elem)) - ( (calcforce elem)-(calcforce elemRight) )

speedDiffAccel = \ elemLeft, elem, elemRight  ->
    elem.damping * (elem.velocity - ( elemLeft.velocity + elemRight.velocity )/2 ) 

calculateShift = \elem ->
    { elem & size : elem.size + elem.velocity * t }

calculateVelocity = \elem, force ->
    { elem & velocity : elem.velocity - (force/elem.mass) * t  }

calcWorldFlow = \ worldIn ->
    if List.isEmpty  worldIn == Bool.false then
        left = List.dropLast (List.prepend worldIn  empty )
        right = List.dropFirst (List.append worldIn  empty )
        forces = List.map3 left worldIn right acceleration  
        speedForce = List.map3 left worldIn right  speedDiffAccel 
        addedForce = List.map2 forces  speedForce  (\ leftSide, rightSide -> leftSide + rightSide )
        List.map2 worldIn addedForce calculateVelocity
        |>   \ worldIns ->  List.map  worldIns calculateShift 

    else
        []
#    
    
#List.map2 ( dropLast  world ) (dropLast world) 
#dropFirst : List elem -> List elem
#dropLast : List elem -> List elem

printElem  = \ elem, tag -> 
    if tag == Size then
        Str.concat (Num.toStr elem.size) " "
    else if tag == Velocity then
        Str.concat (Num.toStr elem.velocity) " "
    else
        ""


printWorld = \ worldIn, tag ->
    List.walk worldIn "" \ str, elem ->
        Str.concat str (printElem elem  tag)

    
iterNode = \ cnt ,k ,m ,d , nodeList-> 
    if cnt == 0 then
        nodeList
    else                         
        List.append nodeList (createElement  m 0.0 0.0  k  d)
        |> (\ updatedNodeList-> iterNode (cnt  - 1 )  k  m d  updatedNodeList )

createWorld = \ data, worldList -> 
    when data is
    [] -> worldList
    [{nodes : n, young : k, mass : m, damping : d}, ..] -> 
        iterNode n  k  m d worldList
        |> (\ modWorldList -> createWorld  (List.dropFirst  data)  modWorldList)  

 

runWorld  = \ iter, worldIn, str ->
    if iter == 0 then
        { world : worldIn , log : str }
    else
        newStr = Str.concat  (Str.concat str  "\n" ) (printWorld  worldIn  Size )
        runWorld (iter - 1 )  (calcWorldFlow  worldIn) newStr 

chengeParam = \ num, val, worldList ->
    when List.get worldList  num  is 
        Ok res -> 
            {res  & size : val.size, velocity : val.velocity }
            |>\ elem ->  List.set  worldList num elem 
        Err _ -> worldList