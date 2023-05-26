interface Parsing
    exposes [setupEnv]
    imports [pf.Stdout]

validate = \ result ->
    when result is
        Ok res -> res
        Err _ -> ""

validateSplit = \ result ->
    when result is
        Ok res -> res
        Err _ -> { before: "", after: "" }

prepareTokens = \ str, tokens ->
    dest = Str.trimLeft str
    when dest is 
        "" -> tokens
        _ -> 
            validated = validateSplit ( Str.splitFirst dest " " )
            prepareTokens validated.after (List.append tokens validated.before )


createElement = \ nodes, young, mass, damping -> 
    {nodes : nodes, young : young, mass : mass, damping : damping}
    
getNumber = \ number ->
    when  number is 
        Ok (Frac val) -> val 
        _ -> 0 
        
appendCondExec = \ activities ->
    if Set.len activities.hot == 0 then
        { activities &  accepted : List.append activities.accepted activities.currSetting, currSetting : Set.empty {}, hot : Set.fromList [Young, Mass, Nodes, Damping, Orange, Blue ] } 
    else
        activities

appendIfNeeded = \ activities, curActiv ->
    dbg  curActiv
    if Bool.isEq  ( Dict.contains curActiv "type" ) Bool.true && 
       Bool.isEq  ( Dict.contains curActiv "value" ) Bool.true then
        activeType = Dict.get curActiv "type"
        dbg  activeType
        when  activeType is 
            Ok (Frac val)  -> activities 
            Ok jena -> 
                secik = Set.fromList [Young, Mass, Nodes, Damping, Orange, Blue ]
                dbg Set.remove (secik) jena
                appendCondExec { activities &  hot : Set.remove activities.hot Nodes ,  currSetting :  Set.insert activities.currSetting   jena}
            _ -> activities 
    else
        activities
# (getNumber (Dict.get curActiv  "value" ))} 

setupEnv = \ str  -> 
    settings = { currSetting : (Set.empty {}),hot :Set.fromList [Young, Mass, Nodes, Damping, Orange, Blue ], accepted : [] }
    prepareTokens (validate  (Str.replaceEach  str "\n" " " ) )  []
    |> ( \ tokens -> createActivities settings tokens  (Dict.empty {}))


readNextToken = \ token, curActiv ->
    empty = Dict.empty {}
    when  token is 
        "young" ->
            Dict.insert empty "type"  Young
        "mass" ->
            Dict.insert empty "type"  Mass
        "nodes" ->
            Dict.insert empty "type"  Nodes
        "damping" ->
            Dict.insert empty "type"  Damping
        "orange" ->
            Dict.insert empty "type"  Orange
        "blue" ->
            Dict.insert empty "type"  Blue
        _ ->
            when  Str.toF32  token is 
                Ok val ->  
                    Dict.insert curActiv "value"  (Frac val)
                Err  _ -> curActiv

createActivities = \ activities, tokens, curActiv ->
    nextToken = List.first tokens
    when nextToken is
        Ok token ->
            modified = readNextToken token curActiv
            createActivities (appendIfNeeded activities  modified) (List.dropFirst  tokens) modified
        Err _ ->
            activities 
            
         

            
createSquare = \ nodes, orange, blue -> 
    {nodes : nodes, orange : orange, blue : blue}    
            
#setupSquaresEnv = \ str  -> 
#    settings = { currSetting : {},hot :Set.fromList [Orange, Blue, Nodes ], accepted : [] }
#    prepareTokens (validate  (Str.replaceEach  str "\n" " " ) )  []
#    |> ( \ tokens -> createActivities settings tokens  (Dict.empty {}))

