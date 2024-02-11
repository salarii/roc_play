interface Solvers
    exposes []
# exposes [tryFindZeroPoint,rkSolver,rkSolverM]
    imports [Matrix.{MatrixType}]

FunctionType  a: ( MatrixType a  ->   a )


# max step needed
# add policy when matrix inversion gives horrible result
tryFindZeroPointInternal : List (FunctionType (Frac a)),MatrixType (Frac a), List (Frac a), MatrixType (Frac a), U32,Frac a -> Result (MatrixType (Frac a)) Str
tryFindZeroPointInternal = \ functions, startPoint, deltas, directions, iterationsCnt ,hitTolerance ->
    funCnt = List.len functions
    deltasSize = List.len deltas
    directionSizes = Matrix.getSize directions

    adjustSize : MatrixType (Frac a), Nat  -> MatrixType (Frac a)
    adjustSize = \ mat, maxSize ->
        # if I try to use deltasSize directly crash!
        if List.len mat >= maxSize then
            List.dropFirst mat ((List.len mat) - maxSize + 1 )
        else
            mat

    # something  is wrong with scopes when I try to use functions there is  a crash
    # I have to pass it as parameter
    createAF : List (FunctionType (Frac a)), MatrixType (Frac a),{x : MatrixType (Frac a), deltaX: MatrixType (Frac a),deltaFun : MatrixType (Frac a)}, Nat -> Result {x : MatrixType (Frac a), deltaX: MatrixType (Frac a),deltaFun : MatrixType (Frac a)} Str
    createAF = \ funLst, pace, statePrev, maxSize ->
        state = { statePrev  &  deltaX: adjustSize statePrev.deltaX maxSize, deltaFun : adjustSize statePrev.deltaFun maxSize}
        if List.len pace == 0 then
            Ok statePrev
        else
            when Matrix.split pace 0 1 is
                Ok paceSplit ->
                    when Matrix.elemWiseOp state.x paceSplit.0 (\a, b -> a + b) is
                        Ok nextX ->
                            when Matrix.merge state.deltaX  paceSplit.0 0 is
                                Ok xDeltaMat ->
                                    when Matrix.elemWiseOp
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

    iteration : List (FunctionType (Frac a)), {x : MatrixType (Frac a), deltaX: MatrixType (Frac a),deltaFun : MatrixType (Frac a)}, U32-> Result (MatrixType (Frac a)) Str
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
                                            minusCorrection = Matrix.scalarOp (Matrix.transpose correction)  -1  Num.mul
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


tryFindZeroPoint : List (FunctionType (Frac a)),MatrixType (Frac a), U32, Frac a -> Result (MatrixType (Frac a)) Str
tryFindZeroPoint = \ functions, startPoint, iterationsCnt ,hitTolerance ->
    size = List.len functions
    pointSize = Matrix.getSize startPoint
    if pointSize.1 != size then
        Err "functions and initial point dimension does not match"
    else
        deltas = List.repeat 0.01 size
        directions = Matrix.unit size
        tryFindZeroPointInternal functions startPoint deltas directions iterationsCnt hitTolerance

# error when:
# rkSolver :  Frac a , Frac a, Frac a, Frac a,Frac a, (Frac a -> Frac a), List (Frac a ) -> List (Frac a)
rkSolver :  Frac a, Frac a, Frac a, Frac a, Frac a, Frac a, (Frac a, Frac a -> Frac a), List (Frac a, Frac a ) -> List (Frac a, Frac a )
rkSolver = \ y, x, h, maxH,err, end , fun, result ->
    if x >= end then
        result
    else
        # again  I have to pass fun as parameter is that correct ??
        evalIncrement : Frac a, Frac a, Frac a, (Frac a, Frac a -> Frac a)-> Frac a
        evalIncrement = \ yi, xi, hi, funi ->
            k1 = hi * (funi xi yi)
            k2 = hi * (funi (xi + 0.5*hi ) (yi + 0.5 * k1))
            k3 = hi * (funi (xi + 0.5*hi ) (yi + 0.5 * k2))
            k4 = hi * (funi (xi + hi ) (yi + k3))
            (k1 + k4) * (1/6) + ( k2 + k3 ) * 2/6

        twoStep =
            evalIncrement  y x (h/2) fun
            |> Num.add y
            |> evalIncrement  x (h/2) fun

        oneStep = evalIncrement  y x h fun
        mod = 1 / ((Num.pow 2 4) - 1)
        estErr =  (twoStep - oneStep) * mod

        if estErr > err then
            rkSolver y x (h/2) maxH err end fun result
        else
            if err * 50 > estErr && maxH > h*2 then
                rkSolver (oneStep + y) (x+h) (h*2) maxH err end fun (List.append result  (x+h ,oneStep + y))
            else
                rkSolver (oneStep + y) (x+h) h maxH err end fun (List.append result  (x+h ,oneStep + y))


rkSolverM :  MatrixType (Frac a), Frac a, Frac a, Frac a, Frac a, Frac a, List (Frac a, MatrixType (Frac a) -> Frac a), List (Frac a, MatrixType (Frac a) ) -> Result ( List (Frac a, MatrixType (Frac a) ) )  Str
rkSolverM = \ y, x, h, maxH,err, end , fun, result ->
    if x >= end then
        Ok result
    else
        # again  I have to pass fun as parameter is that correct ??
        evalIncrement : MatrixType (Frac a), Frac a, Frac a, List (Frac a, MatrixType (Frac a) -> Frac a)-> Result ( MatrixType (Frac a) ) Str
        evalIncrement = \ yi, xi, hi, funLst ->

            k1 = Matrix.scalarOp  [List.map funLst (\ funi -> funi xi yi)]  hi Num.mul
            when (Matrix.elemWiseOp yi (Matrix.scalarOp k1 0.5 Num.mul) Num.add) is
                Ok k1EstimY ->
                    k2 = Matrix.scalarOp
                        [List.map funLst (\ funi ->
                            funi (xi + 0.5*hi ) k1EstimY ) ]
                        hi
                        Num.mul
                    when (Matrix.elemWiseOp yi (Matrix.scalarOp k2 0.5 Num.mul) Num.add) is
                        Ok k2EstimY ->
                            k3 = Matrix.scalarOp
                                [List.map funLst (\ funi ->
                                    funi (xi + 0.5*hi ) k2EstimY ) ]
                                hi
                                Num.mul
                            when  (Matrix.elemWiseOp yi k3 Num.add) is
                                Ok k3EstimY ->
                                    k4 =  Matrix.scalarOp
                                        [List.map funLst (\ funi ->
                                            funi (xi + hi ) k3EstimY ) ]
                                        hi
                                        Num.mul

                                    k14Result = Matrix.elemWiseOp k1  k4 Num.add
                                    k23Result = Matrix.elemWiseOp k2  k3 Num.add
                                    when (k14Result, k23Result) is
                                        (Ok k14,Ok  k23) ->
                                            Matrix.elemWiseOp
                                                (Matrix.scalarOp k14 (1/6) Num.mul)
                                                (Matrix.scalarOp k23 (2/6) Num.mul)
                                                Num.add
                                        _ -> Err "unknown error in rk method"
                                Err message -> Err message
                        Err message -> Err message
                Err message -> Err message

        twoStepResult =
            when evalIncrement y x (h/2) fun is
                Ok halfWay ->
                    when Matrix.elemWiseOp y  halfWay Num.add  is
                        Ok halfY ->
                            evalIncrement halfWay x (h/2) fun
                        Err message -> Err message

                Err  message -> Err  message

        oneStepResult = evalIncrement  y x h fun
        mod = 1 / ((Num.pow 2 4) - 1)
        when (twoStepResult, oneStepResult) is
            (Ok twoStep, Ok oneStep) ->
                estErr =  ((Matrix.norm twoStep) - (Matrix.norm oneStep)) * mod
                if estErr > err then
                    rkSolverM y x (h/2) maxH err end fun result
                else
                    when Matrix.elemWiseOp oneStep y Num.add is
                        Ok currY ->
                            if err * 50 > estErr && maxH > h*2 then
                                rkSolverM currY (x+h) (h*2) maxH err end fun (List.append result  (x+h ,currY))
                            else
                                rkSolverM currY (x+h) h maxH err end fun (List.append result  (x+h ,currY))

                        Err message -> Err message

            _ -> Err "unknown error in rk method"



