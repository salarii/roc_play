interface Parsing
    exposes [setupEnv, checkIfPresent, getTagFromConfig, readCube]
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
    dest = Str.trim str
    when dest is 
        "" -> tokens
        _ -> 
            validated = validateSplit ( Str.splitFirst dest " " )
            prepareTokens validated.after (List.append tokens validated.before )


createElement = \ nodes, young, mass, damping -> 
    {nodes : nodes, young : young, mass : mass, damping : damping}
    


removeFromHot = \ hotSet, elem ->
    when  elem.tag is
        Young -> Set.remove hotSet Young
        Mass -> Set.remove hotSet Mass
        Nodes -> Set.remove hotSet Nodes
        Damping -> Set.remove hotSet Damping
        Orange -> Set.remove hotSet Orange
        Gold -> Set.remove hotSet Gold
        Blue -> Set.remove hotSet Blue
        _ -> hotSet

checkIfPresent = \ list, hotSet  ->
    List.walk list hotSet removeFromHot
    |> Set.len
    |> Bool.isEq 0  

cleanTag = \ tag ->
    when tag  is 
        Value _ -> Value
        Young -> Young
        Mass -> Mass
        Nodes -> Nodes
        Damping -> Damping
        Orange -> Orange
        Gold -> Gold
        Blue -> Blue
        _ -> None


getTagFromConfig = \ tag , config  -> 
    cleanedTag = cleanTag  tag
    when List.findFirst config ( \ elem -> (cleanTag elem.tag) == cleanedTag) is 
        Ok val ->  val 
        Err _ -> { tag: tag, vals : [] }

appendCondExec = \ activities ->
    if checkIfPresent  activities.currSetting  (Set.fromList [Orange, Blue, Nodes ]) == Bool.true ||
       checkIfPresent  activities.currSetting  (Set.fromList [Gold, Nodes ]) == Bool.true then
        
        { activities &  accepted : List.append activities.accepted activities.currSetting, currSetting : [] } 
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
        Value val ->
            { activities & currSetting : updateLast activities.currSetting val }
        _ -> 
            updated = appendCondExec  activities
            { updated & currSetting : List.append updated.currSetting { tag: token, vals : [] } }

setupEnv = \ str  -> 
    settings = { currSetting : [], accepted : [] }
    prepareTokens (Str.replaceEach  str "\n" " " )  []
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
        "gold" ->
            Gold
        "orange" ->
            Orange
        "blue" ->
            Blue
        _ ->
            when  Str.toF32  token is 
                Ok val ->  
                    (Value  val)
                Err  _ -> None




createActivities = \ activities, tokens ->
    nextToken = List.first tokens
    when nextToken is
        Ok token ->
            modified = readNextToken token
            createActivities  (appendIfNeeded activities  modified)  (List.dropFirst  tokens) 
        Err _ ->
            appendIfNeeded activities PushLastToken

readCubeFromTokens = \ tokens, cubex, cubexy, cube  ->    
    nextToken = List.first tokens
    when nextToken is
        Ok "y" ->
                readCubeFromTokens (List.dropFirst  tokens) [] (List.append cubexy cubex ) cube
        Ok "z" ->
                readCubeFromTokens (List.dropFirst  tokens) [] [] (List.append cube cubexy )
        Ok val ->
                when  Str.toF32  val is 
                    Ok val32 ->  
                        
                        readCubeFromTokens  (List.dropFirst  tokens) (List.append cubex val32) cubexy cube
                    Err  _ -> [] 

        Err _ -> cube


readCube = \ str -> 
    Parsing.prepareTokens    (Str.replaceEach  str "\n" " "  )   [] 
    |> readCubeFromTokens [] [] []
            
createSquare = \ nodes, orange, blue -> 
    {nodes : nodes, orange : orange, blue : blue}    
            
#setupSquaresEnv = \ str  -> 
#    settings = { currSetting : {},hot :Set.fromList [Orange, Blue, Nodes ], accepted : [] }
#    prepareTokens (validate  (Str.replaceEach  str "\n" " " ) )  []
#    |> ( \ tokens -> createActivities settings tokens  (Dict.empty {}))

