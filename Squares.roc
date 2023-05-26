interface Squares
    exposes [calcWorldFlow,runWorld]
    imports []
    

#  a  =  dt/dx *O
#  b  =  dt/dx *B
# dO/dt = O * dB/dx
oGlob = 10.0 

bGlob = 1.0

dtGlob = 0.01


createOrangeElement = \ orange -> 
    {orange : orange}
createBlueElement = \ blue -> 
    { blue : blue}
    
    
empty = createBlueElement  0.0

determineMid = \ inSequence ->
    List.dropFirst inSequence
    |> List.map2 (List.dropLast inSequence) (\left, right -> (left + right)/2)
    
determineRight = \ inSequence, mids, mod ->
    List.dropLast inSequence
    |> List.map2 mids (\left, right -> ( right - left ) *mod)

determineLeft = \ inSequence, mids, mod ->
    List.dropFirst inSequence
    |> List.map2 mids (\left, right -> (left - right) *mod)

gatherOrange = \ squares ->
    List.map squares (\elem -> elem.orange) 
    
gatherBlue = \ squares ->
    List.map squares (\elem -> elem.blue)

printElem  = \ lista -> 
    List.walk lista "" \ str, elem ->
        Str.concat str " " 
        |> Str.concat (Num.toStr elem)


printWorld = \ worldIn, tag ->
    if tag == Orange then
        printElem  (gatherOrange worldIn.oranges)
    else if tag == Blue then
        printElem  (gatherBlue worldIn.blues)
    else
        ""



runWorld  = \ iter, worldIn, str ->
    if iter == 0 then
        { world : worldIn , log : str }
    else
        newStr = Str.concat  (Str.concat str  "\n" ) (printWorld  worldIn  Blue )
        runWorld (iter - 1 )  (Squares.calcWorldFlow  worldIn) newStr 




update = \ squaresIn, oranges, blues ->
    { blues : (List.map2 squaresIn.blues blues (\ elem, val -> {blue : elem.blue + dtGlob*val})),
      oranges : (List.map2 squaresIn.oranges oranges (\ elem, val -> {orange : elem.orange + dtGlob*val}))}
    
calcWorldFlow = \ squaresIn ->
    if ((List.isEmpty  squaresIn.blues) == Bool.false) && ((List.isEmpty  squaresIn.oranges) == Bool.false) then
        blues = gatherBlue squaresIn.blues
        mids = determineMid blues   
        orangeDeltas = List.map2 (determineLeft blues mids 1) (determineRight blues mids 1) (\left, right -> left + right)
        gatherOrange squaresIn.oranges
        |> List.prepend   0.0
        |> List.append  0.0 
        |>(\ orangesPrepApp -> List.map2 (determineLeft orangesPrepApp ( determineMid orangesPrepApp ) 1) (determineRight orangesPrepApp ( determineMid orangesPrepApp ) 1) (\left, right -> left + right))
        |> (\ blueDeltas -> update squaresIn  orangeDeltas  blueDeltas )
    else
        {blues: [], oranges :[]}
