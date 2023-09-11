interface Util
    exposes [getFromList, getListFromList, createNode, createNodeAni]
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