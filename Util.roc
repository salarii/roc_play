interface Util
    exposes [getFromList, getListFromList, createNode]
    imports []
    
    
getFromList = \ list, cnt  ->
    when List.get list cnt  is 
        Ok val -> val
        Err _ -> 0.0f32


getListFromList = \ list, cnt  ->
    when List.get list cnt  is 
        Ok val -> val
        Err _ -> []
        
        
createNode = \ val, param->
    {value : val, param : param}