interface Util
    exposes [getFromList, getListFromList, createNode, createNodeAni,
            printCubes, getRelevant]
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
    {value : val, param : param, omega : omega }
    
createNodeAni = \ val, param, omegaX, omegaY, omegaZ ->
    {value : val, param : param, omega : {x : omegaX, y: omegaY, z: omegaZ} }
    
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
        
        