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


removeFromHot \ hotSet, elem ->
    Set.remove hotSet elem.tag

checkIfPresent  \ list, hotSet  ->
    List.walk settings, hotSet, removeFromHot
    |> Set.len
    |> Bool.isEq 0  



appendCondExec = \ activities ->
    if checkIfPresent  activities.currSetting  (Set.fromList [Orange, Blue, Nodes ])  then
        { activities &  accepted : List.append activities.accepted activities.currSetting, currSetting : Dict.empty {}, hot : Set.empty {} } 
    else
        activities

updateLast = \ listos, val ->
    when List.last listos is 
        Ok  elem ->  
            updated = { elem & vals : List.append elem.vals  val }
            List.dropLast  listos
            |> List.append updated
        Err _ -> listos

appendIfNeeded = \ activities, token ->
    when  token is
        Frac val ->
            { activities & currSetting : updateLast activities.currSetting val }
        _ -> 
            { activities & currSetting : List.append activities.currSetting { tag: token, vals : [] } }

setupEnv = \ str  -> 
    settings = { currSetting : [], accepted : [] }
    prepareTokens (validate  (Str.replaceEach  str "\n" " " ) )  []
    |> ( \ tokens -> createActivities settings tokens  )


readNextToken = \ token ->
    when  token is 
        "young" ->
            Young
        "mass" ->
            Mass
        "nodes" ->
            Nodes
        "damping" ->
            Damping
        "orange" ->
            Orange
        "blue" ->
            Blue
        _ ->
            dbg  token
            when  Str.toF32  token is 
                Ok val ->  
                    (Frac val)
                Err  _ -> None

createActivities = \ activities, tokens ->
    nextToken = List.first tokens
    dbg activities
    when nextToken is
        Ok token ->
            dbg  nextToken
            modified = readNextToken token
            createActivities  (appendIfNeeded activities  modified)  (List.dropFirst  tokens) 
            #createActivities  (List.dropFirst  tokens) modified
        Err _ ->
            dbg  activities
            activities 
            
         

            
createSquare = \ nodes, orange, blue -> 
    {nodes : nodes, orange : orange, blue : blue}    
            
#setupSquaresEnv = \ str  -> 
#    settings = { currSetting : {},hot :Set.fromList [Orange, Blue, Nodes ], accepted : [] }
#    prepareTokens (validate  (Str.replaceEach  str "\n" " " ) )  []
#    |> ( \ tokens -> createActivities settings tokens  (Dict.empty {}))

