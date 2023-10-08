interface Util
    exposes [getFromList, getListFromList, createNode, createNodeAni,
            printCubes, getRelevant, makeStringCube, makeStringSq]
    imports []
    
    
getFromList = \ list, cnt, def  ->
    when List.get list cnt  is 
        Ok val -> val
        Err _ -> def


getListFromList = \ list, cnt  ->
    when List.get list cnt  is 
        Ok val -> val
        Err _ -> []
        
        
createNode = \ val, param, omega->
    {value : Num.toF32 val, param : Num.toF32 param, omega : Num.toF32 omega }
    
createNodeAni = \ val, param, omegaX, omegaY, omegaZ ->
    {value : Num.toF32 val, param : Num.toF32 param, omega : {x : Num.toF32 omegaX, y: Num.toF32 omegaY, z: Num.toF32 omegaZ} }
    
getRelevant = \ elem ->
    Num.toStr elem.value

makeStringCube = \ cube, getRelevantCb, separator-> 
    List.walk cube  ""  ( \ str, sq  ->  Str.concat str (Str.concat ( makeStringSq sq getRelevantCb separator.y ) separator.z ) )
     
makeStringSq =  \ square, getRelevantCb,separator -> 
    List.walk square  "" (\ str, list ->
                            List.walk list str (\strIn, val -> Str.concat (Str.concat strIn  " ") (  getRelevantCb val ) )
                            |> Str.concat  separator )


printCubes = \ cubes, pat ->
    List.walk cubes  "" 
    ( \ state, cube   -> state
        |> Str.concat ( makeStringCube cube getRelevant  pat ) )
        
        