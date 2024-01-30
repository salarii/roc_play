interface Solvers
    exposes [tryFindZeroPoint]
    imports [Matrix.{MatrixType}]

FunctionType a : ( MatrixType a  -> Frac a )


# max step needed
# add policy when matrix inversion gives horrible result
tryFindZeroPointInternal : List (FunctionType a),MatrixType a, List (Frac a), MatrixType a, U32,Frac a -> Result (MatrixType a) Str
tryFindZeroPointInternal = \ functions, startPoint, deltas, directions, iterationsCnt ,hitTolerance ->
    funCnt = List.len functions
    deltasSize = List.len deltas
    directionSizes = Matrix.getSize directions

    adjustSize : MatrixType a, Nat  -> MatrixType a
    adjustSize = \ mat, maxSize ->
        # if I try to use deltasSize directly crash!
        if List.len mat >= maxSize then
            List.dropFirst mat ((List.len mat) - maxSize + 1 )
        else
            mat

    # something  is wrong with scopes when I try to use functions there is  a crash
    # I have to pass it as parameter
    createAF : List (FunctionType a), MatrixType a,{x : MatrixType a, deltaX: MatrixType a,deltaFun : MatrixType a}, Nat -> Result {x : MatrixType a, deltaX: MatrixType a,deltaFun : MatrixType a} Str
    createAF = \ funLst, pace, statePrev, maxSize ->
        state = { statePrev  &  deltaX: adjustSize statePrev.deltaX maxSize, deltaFun : adjustSize statePrev.deltaFun maxSize}
        if List.len pace == 0 then
            Ok statePrev
        else
            when Matrix.split pace 0 1 is
                Ok paceSplit ->
                    when Matrix.matrixElemOp state.x paceSplit.0 (\a, b -> a + b) is
                        Ok nextX ->
                            when Matrix.merge state.deltaX  paceSplit.0 0 is
                                Ok xDeltaMat ->
                                    when Matrix.matrixElemOp
                                        [List.map funLst (\ fun ->  fun nextX )]
                                        [List.map funLst (\ fun ->  fun state.x )]
                                        (\a, b -> a - b) is
                                        Ok funDelta ->
                                            when Matrix.merge state.deltaFun funDelta 0  is
                                                Ok funDeltaMat ->
                                                    createAF funLst paceSplit.1  {x : nextX, deltaX: xDeltaMat, deltaFun : funDeltaMat} maxSize
                                                Err message -> Err message
                                        Err message -> Err message
                                Err message -> Err message
                        Err message -> Err message
                Err message -> Err message

    iteration : List (FunctionType a), {x : MatrixType a, deltaX: MatrixType a,deltaFun : MatrixType a}, U32-> Result (MatrixType a) Str
    iteration = \ funLst, state, iter ->
        if  iter == 0 then
            Ok state.x
        else
            List.walkUntil (List.map funLst (\ fun ->  fun state.x ))  Bool.true (\hit,  val ->
                if ( Num.abs val > Num.abs hitTolerance) then
                    Break Bool.false
                else
                    Continue hit
                 )
            |>  (\ hit ->
                if hit == Bool.true then
                    Ok state.x
                else
                    when Matrix.inverse (Matrix.transpose state.deltaFun) is
                        Ok inverse ->
                            when Matrix.mul (Matrix.transpose state.deltaX) inverse is
                                Ok aMat ->
                                    when Matrix.mul aMat (Matrix.transpose [List.map functions (\ fun ->  fun state.x )]) is
                                        Ok correction ->
                                            minusCorrection = Matrix.scalarOp (Matrix.transpose correction)  -1  ( \a,b -> a*b )
                                            when createAF funLst minusCorrection state deltasSize is
                                                Ok afUpdated ->
                                                    iteration funLst {x : afUpdated.x, deltaX: afUpdated.deltaX, deltaFun : afUpdated.deltaFun } (iter - 1)
                                                Err message -> Err message
                                        Err message -> Err message
                                Err message -> Err message
                        Err message -> Err message )
    paceMat =
        List.map2 directions deltas (\ row, val -> (List.map  row (\ elem -> elem * val)) )

    when createAF functions paceMat {x : startPoint, deltaX: [], deltaFun : []  } deltasSize is
        Ok af ->
            iteration functions af iterationsCnt
        Err message -> Err message


tryFindZeroPoint : List (FunctionType a),MatrixType a, U32, Frac a -> Result (MatrixType a) Str
tryFindZeroPoint = \ functions, startPoint, iterationsCnt ,hitTolerance ->
    size = List.len functions
    pointSize = Matrix.getSize startPoint
    if pointSize.1 != size then
        Err "functions and initial point dimension does not match"
    else
        deltas = List.repeat 0.01 size
        directions = Matrix.unit size
        tryFindZeroPointInternal functions startPoint deltas directions iterationsCnt hitTolerance
