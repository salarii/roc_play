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


makeSquare = \ rows, cols, list  ->
    if rows == 0 then
        list
    else
        makeSquare (rows - 1) cols  (List.append  list (List.repeat  0  cols ))

makeString =  \ square -> 
    List.walk square  "" (\ str, list -> List.walk list (Str.concat  str "\n") (\strIn, val -> Str.concat (Str.concat strIn  " ") ( Num.toStr val ) ) )

modifyField  = \ square, row, col, val ->
    getListFromList square row
    |> List.replace col val 
    |> (\ updated -> (List.replace square row updated.list).list  )

secondDerivUpDown = \ lists,  delta, edges  -> 
    len = List.len (getListFromList  lists 0) 
    listScrollUp = List.dropLast ( List.prepend  lists (List.repeat edges.up len) )
    listScrollDown = List.dropFirst (List.append  lists (List.repeat edges.up len) )

    List.map3 listScrollUp lists listScrollDown  ( \   upList, midList, downList ->
                                                              dbg  upList
                                                              dbg  midList
                                                              dbg  downList
                                                              essenceDerivOp upList midList downList delta)
    

solution = \ list, prevList, deltaTime, deltaSpace    -> 
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
   
main =
    
    #list  =  List.map  ( modList  (centerList points) cosMod ) ( \ val -> Num.cos(  val ) ) 
    squar =  makeSquare  5  10  [] 
    deriv  =  secondDerivUpDown  ( modifyField  squar  2  2  1 )  1  { up: 0 , down: 0 }
    dbg  secondDeriv  [ 0, 0,  1, 0, 0]  1  {  right : 0 , left : 0}
    #dbg  calculateSolution list  list  iter 
    
    #dbg     dbg shift [1, 2, 3, 4] Right 0  2
     #eton =  Parsing.setupEnv  "nodes 2 20 \n gold  0  \n "
    #dbg  eton 
    #dbg createWorld (eton.accepted )

    Stdout.line  ( makeString deriv)
    
    
    
