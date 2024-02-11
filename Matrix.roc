interface Matrix
    exposes [
        print,
        printC,
        solve,
        solveC,
        mul,
        mulC,
        transpose,
        elemWiseOp,
        create,
        merge,
        inverse,
        inverseC,
        getSize,
        split,
        scalarOp,
        createSingleValue,
        norm,
        unit,
        findEigenValues,
        findEigenValuesC,
        qrDecompositionC,
        powerMethodC,
        ]
    imports [ Complex.{ComplexType} ]

MatrixType a : List ( List (a))
# I can't just use it when it applies.
compSet = {rtol :0.0001 ,atol : 0.00001 }

getMatrixValue : MatrixType a, Nat, Nat -> Result a Str
getMatrixValue = \ mat, row, col ->
    when List.get mat row is
        Ok rowLst ->
            when List.get rowLst col is
                Ok val -> Ok val
                _ -> Err "wrong col index"
        _ -> Err "wrong row index"


getSize : MatrixType a -> (Nat, Nat)
getSize = \ mat ->
    when mat is
        [] -> (0,0)
        [head, .. as tail] ->
            ( List.len mat, List.len head )

solve : MatrixType (Frac a), MatrixType (Frac a) -> Result (MatrixType (Frac a)) Str
solve = \  aMat, bMat ->
    op = {
        greater : (\ a, b ->  Num.isGt (Num.abs a) (Num.abs b)),
        equal : (\ a, b -> Num.isApproxEq  a b {rtol :0.0001 ,atol : 0.00001  }),
        isZero : Num.isZero,
        sub : Num.sub ,
        mul : Num.mul,
        div : Num.div,
        neg : (\a -> -a),
        sum : (\a, b -> a + b),
        sumLst : (\ lst -> List.sum lst),
        len : (\ lst ->
            List.map lst (\ a -> a * a )
            |> List.sum
            |> Num.sqrt
            ),
    }
    solveInternal aMat bMat op
    # as far as I am concerned this should work
    # otential bug
    #solveInternal a b opFrec

solveC : MatrixType (ComplexType a), MatrixType (ComplexType a) -> Result (MatrixType (ComplexType a)) Str
solveC = \  aMat, bMat ->
    op = {
        greater : (\ a, b ->  Num.isGt ((Num.abs a.0) + (Num.abs a.1)) ((Num.abs b.0) + (Num.abs b.1))),
        equal : (\ a, b -> (Num.isApproxEq  a.0 b.0 {rtol :0.0001 ,atol : 0.00001  }) && (Num.isApproxEq  a.1 b.1 {rtol :0.0001 ,atol : 0.00001  })),
        isZero : (\ a -> ( Num.isZero  a.0 ) && ( Num.isZero  a.1 )),
        sub : Complex.sub,
        mul : Complex.mul,
        div : Complex.div,
        neg : Complex.neg,
        sum : Complex.add,
        sumLst : (\ lst -> List.walk lst (0.0,0.0)
            (\ state, elem->
                (state.0 + elem.0, state.1 + elem.1)  ) ),
        len : (\ lst ->
            len =
                List.map lst (\ a ->
                    (Complex.mul a (Complex.conj a )).0
                    |> Num.abs )
                |> List.sum
                |> Num.sqrt
            (len, 0)
            ),
    }
    solveInternal aMat bMat op

solveInternal : MatrixType a, MatrixType a, OperationType a -> Result (MatrixType a) Str
solveInternal = \  a, b, op ->
    sizeA = getSize a
    sizeB = getSize b

    if (sizeA.0 != sizeB.0 && sizeA.0 != sizeB.1) ||
       sizeA.0 != sizeA.1 then
        Err "wrong matrix sizes, can't solve"
    else
        updatedB =
            if sizeA.0 == sizeB.0 then
                b
            else
                transpose b
        when merge a updatedB 1 is
            Ok glued ->
                when gaussElimination glued op is
                    Ok gauss ->
                        when split gauss 1 sizeA.0 is
                            Ok splited ->
                                solveFromGauss splited.1 splited.0 op
                            Err message -> Err message
                    Err message -> Err message
            Err message -> Err message

transpose : MatrixType a -> MatrixType a
transpose =\ mat ->
    when mat is
        [] ->
            mat
        [head, .. as tail ] ->
            List.walk tail (List.chunksOf head 1) ( \ state, row ->
                List.map2 state  (List.chunksOf row 1) ( \ left, right ->
                    List.concat left right ) )

merge : MatrixType a, MatrixType a, Nat -> Result (MatrixType a) Str
merge = \ dest, src, dim ->
    sizeDest = getSize dest
    sizeSrc = getSize src
    if dim == 0 then
        if sizeDest.1 == 0 || sizeDest.1 == sizeSrc.1 then
            Ok (List.concat dest src)
        else
            Err "Incompatible sizes can't merge"
    else if dim == 1 then
        if sizeDest.0 == sizeSrc.0 then
            Ok (
                List.map2 dest src ( \ left, right ->
                    List.concat  left right ) )
        else if sizeDest.0 == 0 then
            Ok ( src )
        else
            Err "Incompatible sizes can't merge"

    else
        Err "inproper dimension"

 split : MatrixType a, Nat, Nat -> Result (MatrixType a,MatrixType a) Str
 split = \ mat, dim, cnt ->
    if dim == 0 then

        if List.len mat < cnt then
            Err "can't split that way, wrong position"
        else
            Ok (List.takeFirst mat cnt, List.takeLast mat ((List.len mat) - cnt ))
    else if dim == 1 then
        List.walkTry mat ([],[]) ( \ state, row ->
            if List.len row < cnt then
                Err "can't split that way, wrong position"
            else
                Ok (
                    List.append state.0 (List.takeFirst row cnt),
                    List.append state.1 (List.takeLast row ((List.len row) - cnt ))
                ))
    else
        Err "inproper dimension"
mul : MatrixType (Frac a), MatrixType (Frac a) -> Result (MatrixType (Frac a)) Str
mul = \ left, right ->
    op = {
        greater : (\ a, b ->  Num.isGt (Num.abs a) (Num.abs b)),
        equal : (\ a, b -> Num.isApproxEq  a b {rtol :0.0001 ,atol : 0.00001  } ),
        isZero : Num.isZero,
        sub : Num.sub ,
        mul : Num.mul,
        div : Num.div,
        neg : (\a -> -a),
        sum : (\a, b -> a + b),
        sumLst : (\ lst -> List.sum lst),
        len : (\ lst ->
            List.map lst (\ a -> a * a )
            |> List.sum
            |> Num.sqrt
            ),
    }

    mulInternal left right op

mulC : MatrixType (ComplexType a), MatrixType (ComplexType a) -> Result (MatrixType (ComplexType a)) Str
mulC = \ left, right ->
    # again horrible ,why I have to do that??
    # I want to asign opComplex or opFrec here
    op = {
        greater : (\ a, b ->  Num.isGt ((Num.abs a.0) + (Num.abs a.1)) ((Num.abs b.0) + (Num.abs b.1))),
        equal : (\ a, b -> (Num.isApproxEq  a.0 b.0 {rtol :0.0001 ,atol : 0.00001  }) && (Num.isApproxEq  a.1 b.1 {rtol :0.0001 ,atol : 0.00001  })),
        isZero : (\ a -> ( Num.isZero  a.0 ) && ( Num.isZero  a.1 )),
        sub : Complex.sub,
        mul : Complex.mul,
        div : Complex.div,
        neg : Complex.neg,
        sum : Complex.add,
        sumLst : (\ lst -> List.walk lst (0.0,0.0)
            (\ state, elem->
                (state.0 + elem.0, state.1 + elem.1)  ) ),
        len : (\ lst ->
            len =
                List.map lst (\ a ->
                    (Complex.mul a (Complex.conj a )).0
                    |> Num.abs )
                |> List.sum
                |> Num.sqrt
            (len, 0)
            ),
    }

    mulInternal left right op


mulInternal : MatrixType a, MatrixType a, OperationType a -> Result (MatrixType a) Str
mulInternal = \ left, right, op ->
    sizeLeft = getSize left
    sizeRigh = getSize right

    if sizeLeft.1 == sizeRigh.0 then
        transp = transpose right
        Ok
            (List.walk left [] ( \ outMat, row ->
                List.append outMat (
                    (List.walk transp [] ( \ outRow, col ->
                        List.append  outRow (
                        List.map2 row  col (\ elem1, elem2 ->
                            op.mul elem1 elem2
                        )
                        |> op.sumLst )
                    )) )
            ) )
    else
        Err "Incompatible sizes"


printC : MatrixType (ComplexType a) -> Str
printC = \ mat ->
    strMat =
        List.map mat ( \ row ->
            List.map row  (\ elem ->
                (Complex.print  elem )
            )
        )

    List.walk strMat "" (\ outStr, row ->
        List.walk row (Str.concat outStr "\n")  (\ inStr, str ->
            inStr
            |> Str.concat str
            |> Str.concat "  "
        )
    )

print : MatrixType (Frac a) -> Str
print = \ mat ->
    adjustFront : Str, Str, Nat -> Str
    adjustFront = \ str, filler, cnt ->
        Str.concat ( Str.repeat  filler  cnt) str

    strRound : Frac a, Frac a -> Str
    strRound = \ val, round ->

        rounded =
            Num.toStr (Num.floor ( ( ( (Num.abs val) - (Num.toFrac (Num.floor (Num.abs val))))) * (Num.pow 10.0 round)))
            |> ( \ restStr ->
                adjustFront restStr "0" ((Num.toNat (Num.round round)) - (List.len  (Str.toUtf8 restStr)) ))

        valueStr =
            if Num.isNegative val then
                str = Num.toStr (Num.ceiling val)
                if (Num.ceiling val) == 0 then
                    Str.concat "-" str
                else
                    str
            else
                Num.toStr (Num.floor val)

        valueStr
        |> Str.concat "."
        |> Str.concat rounded

    strMat =
        List.map mat ( \ row ->
            List.map row  (\ elem ->
                (strRound  elem  2)
            )
        )

    longest = List.walk (List.join strMat) 0 ( \ len, str ->
        Num.max (List.len (Str.toUtf8 str)) len
    )

    List.walk strMat "" (\ outStr, row ->
        List.walk row (Str.concat outStr "\n")  (\ inStr, str ->
            inStr
            |> Str.concat (adjustFront str " "  (longest - (List.len (Str.toUtf8 str) )))
            |> Str.concat "  "
        )
    )

inverse : MatrixType (Frac a) -> Result (MatrixType (Frac a))  Str
inverse = \ mat ->
    # again horrible ,why I have to do that??
    # I want to asign opComplex or opFrec here
    op = {
        greater : (\ a, b ->  Num.isGt (Num.abs a) (Num.abs b)),
        equal : (\ a, b -> Num.isApproxEq  a b {rtol :0.0001 ,atol : 0.00001  } ),
        isZero : Num.isZero,
        sub : Num.sub ,
        mul : Num.mul,
        div : Num.div,
        neg : (\a -> -a),
        sum : (\a, b -> a + b),
        sumLst : (\ lst -> List.sum lst),
        len : (\ lst ->
            List.map lst (\ a -> a * a )
            |> List.sum
            |> Num.sqrt
            ),
    }
    inverseInternal mat (Num.toFrac 0) (Num.toFrac 1) op

inverseC : MatrixType (ComplexType a) -> Result (MatrixType (ComplexType a))  Str
inverseC = \ mat ->
    # this is horrible that I have to repeat this over and over
    op = {
        greater : (\ a, b ->  Num.isGt ((Num.abs a.0) + (Num.abs a.1)) ((Num.abs b.0) + (Num.abs b.1))),
        equal : (\ a, b -> (Num.isApproxEq  a.0 b.0 {rtol :0.0001 ,atol : 0.00001  }) && (Num.isApproxEq  a.1 b.1 {rtol :0.0001 ,atol : 0.00001  })),
        isZero : (\ a -> ( Num.isZero  a.0 ) && ( Num.isZero  a.1 )),
        sub : Complex.sub,
        mul : Complex.mul,
        div : Complex.div,
        neg : Complex.neg,
        sum : Complex.add,
        sumLst : (\ lst -> List.walk lst (0.0,0.0)
            (\ state, elem->
                (state.0 + elem.0, state.1 + elem.1)  ) ),
        len : (\ lst ->
            len =
                List.map lst (\ a ->
                    (Complex.mul a (Complex.conj a )).0
                    |> Num.abs )
                |> List.sum
                |> Num.sqrt
            (len, 0)
            ),
    }
    inverseInternal mat (Num.toFrac 0,Num.toFrac 0) (Num.toFrac 1,Num.toFrac 0) op

inverseInternal : MatrixType a, a, a, OperationType a -> Result (MatrixType a)  Str
inverseInternal = \ mat, zero, one, op ->
    size = getSize mat
    if size.0 == size.1 then
        iterate : MatrixType a, Nat -> Result (MatrixType a)  Str
        iterate = \ out, cnt ->
            if cnt == List.len mat then
                Ok out
            else
                current =
                    List.repeat zero size.1
                    |> List.set cnt one
                when (create [current] (\a -> a)) is
                    Ok iterB ->
                        when solveInternal mat iterB op is
                            Ok result ->
                                when out is
                                    [] -> iterate (transpose result) (cnt + 1)
                                    _ ->
                                        when merge out (transpose result) 1 is
                                            Ok toOut ->
                                                iterate toOut (cnt + 1)
                                            Err message -> Err message
                            Err message -> Err message
                    Err  message -> Err message
        iterate [] 0
    else
        Err "matrix must be square"

create : List (List a), (a -> b) -> Result (MatrixType b) Str
create = \ seedLst, convert ->
    conversion : (List  a) -> List b
    conversion = \ lst ->
        List.map lst (\elem -> convert elem )

    List.walkTry seedLst [] ( \ mat, row  ->
        when mat  is
            [] -> Ok  [conversion row]
            [ .. as head, last ] ->
                if List.len last == List.len row then
                    Ok ( List.append mat ( conversion row) )
                else
                    Err "wrong size can't create matrix"
    )

createSingleValue : a, Nat, Nat -> MatrixType a
createSingleValue = \ val, rowCnt, colCnt ->
    List.repeat (List.repeat val colCnt) rowCnt

scalarOp : MatrixType a,  a, ( a,  a ->  a) -> MatrixType a
scalarOp = \ mat, val, op ->
    List.map mat ( \ row ->
        List.map row ( \ elem -> op elem val ) )

elemWiseOp : MatrixType a, MatrixType a, ( a,  a ->  a) -> Result (MatrixType a) Str
elemWiseOp = \ left, right, op ->
    if getSize left != getSize right then
        Err "wrong matrices sizes"
    else
        Ok (
            List.map2 left right (\ leftRow, rightRow ->
                List.map2 leftRow  rightRow (\ leftElem, rightElem ->
                    op leftElem rightElem
                )
        ))

norm : MatrixType (Frac a) -> Frac a
norm = \ mat ->
    List.map  mat  (\ row  ->
        List.map row (\val -> Num.abs val )
        |> List.sum
    )
    |> List.sum

unit : Nat -> MatrixType (Frac a)
unit = \ size ->
    unitInternal size (Num.toFrac 0) (Num.toFrac 1)

unitInternal : Nat, a, a -> MatrixType a
unitInternal = \ size, zero, one ->
    row =
        List.concat  [one]  (List.repeat zero (size - 1))

    replicate : MatrixType a, List a , Nat -> MatrixType a
    replicate = \ in, currentRow,  cnt ->
        if cnt == 0 then
            in
        else
            modified = List.swap currentRow ((List.len currentRow) - cnt) ((List.len currentRow) - cnt - 1)
            List.append in modified
            |> replicate modified (cnt - 1)

    replicate [row] row  (size - 1)

OperationType a: {
    greater : (a, a -> Bool),
    equal : (a, a -> Bool),
    isZero : (a -> Bool),
    sub : (a, a -> a),
    mul : (a, a -> a),
    div : (a, a -> a),
    neg : (a -> a),
    sum : (a, a -> a),
    sumLst : (List a ->  a ),
    len : (List a -> a ),
}

opFrec : OperationType (Frac a)
opFrec = {
    greater : (\ a, b ->  Num.isGt (Num.abs a) (Num.abs b)),
    equal : (\ a, b -> Num.isApproxEq  a b {rtol :0.0001 ,atol : 0.00001  } ),
    isZero : Num.isZero,
    sub : Num.sub ,
    mul : Num.mul,
    div : Num.div,
    neg : (\a -> -a),
    sum : (\a, b -> a + b),
    sumLst : (\ lst -> List.sum lst),
    len : (\ lst ->
        List.map lst (\ a -> a * a )
        |> List.sum
        |> Num.sqrt
        ),
}

opComplex : OperationType (ComplexType a)
opComplex = {
    greater : (\ a, b ->  Num.isGt ((Num.abs a.0) + (Num.abs a.1)) ((Num.abs b.0) + (Num.abs b.1))),
    equal : (\ a, b -> (Num.isApproxEq  a.0 b.0 {rtol :0.0001 ,atol : 0.00001  }) && (Num.isApproxEq  a.1 b.1 {rtol :0.0001 ,atol : 0.00001  })),
    isZero : (\ a -> ( Num.isZero  a.0 ) && ( Num.isZero  a.1 )),
    sub : Complex.sub,
    mul : Complex.mul,
    div : Complex.div,
    neg : Complex.neg,
    sum : Complex.add,
    sumLst : (\ lst -> List.walk lst (0.0,0.0)
        (\ state, elem->
            (state.0 + elem.0, state.1 + elem.1)  ) ),
    len : (\ lst ->
        len =
            List.map lst (\ a ->
                (Complex.mul a (Complex.conj a )).0
                |> Num.abs )
            |> List.sum
            |> Num.sqrt
        (len, 0)
        ),
}

gaussElimination : MatrixType a, OperationType a -> Result (MatrixType a) Str
gaussElimination = \ mat, op ->
    gaussEliminationInternal mat [] op

gaussEliminationInternal : MatrixType a, MatrixType a, OperationType a -> Result (MatrixType a) Str
gaussEliminationInternal = \ mat, tmp, operations ->

    sorter : List a, List  a, OperationType a -> [ LT, EQ, GT ]
    sorter = \ left, right, op ->
            when (List.get left line, List.get right line) is
                (Ok leftElem, Ok  rightElem) ->
                    if (op.greater rightElem leftElem) then
                        GT
                    else if op.equal rightElem leftElem then
                        EQ
                    else
                        LT
                _ -> GT

    line = List.len tmp
    sortedMat = List.sortWith mat (\ a, b -> sorter a b operations )
    when sortedMat is
        [] -> Ok tmp
        # [last] ->
        #     Ok (List.append tmp last)
        [head ,.. as tail] ->
            when List.get head line is
                Ok active ->
                    updatedHead = (List.map head ( \ elem ->
                                    operations.div elem active ))
                    List.walkTry tail [] (\ state, arr ->
                        when List.get arr line is
                                Ok pasive ->
                                    if operations.isZero active then
                                        Err "operation can't be performed on this matrix"
                                    else
                                        modifier = pasive

                                        Ok (List.append state (
                                                (List.map2 arr updatedHead ( \ pasiveElem, activeElem ->

                                                        operations.sub pasiveElem (operations.mul modifier activeElem)
                                                    ))))

                                _ -> Err "operation can't be performed on this matrix")
                    |> ( \ processingResult ->
                        when processingResult is
                            Ok processed ->

                                gaussEliminationInternal processed (List.append tmp updatedHead) operations
                            Err message -> Err message
                    )
                _ -> Err "operation can't be performed on this matrix"

solveFromGauss : List (List a), List (List a), OperationType a -> Result (List (List a)) Str
solveFromGauss = \ b, gauss, operations ->
    last = List.len (List.join b)
    List.walkTry (List.reverse  (List.join b)) [] ( \ state, bi ->
        idx = (last - 1 - (List.len state))
        when List.get gauss idx is
            Ok row ->
                when  List.get row (idx) is
                    Ok aii ->
                        List.map2 state (List.reverse row) ( \ xi, ai  ->
                            operations.mul (operations.neg xi) ai
                        )
                        |> operations.sumLst
                        |> ( \ other ->
                            Ok (List.append  state (operations.div (operations.sum bi other) aii))
                        )
                    _ -> Err "unexpected size problem"

            _ -> Err "not compatible sizes"
    )
    |> (\ unboxed ->
        when unboxed is
            Ok unbox -> Ok [ List.reverse unbox]
            Err message -> Err message)


qInternal : MatrixType a, OperationType a -> MatrixType a
qInternal = \  mat, operations ->
    transp = Matrix.transpose mat

    List.walk transp [] ( \ state, vec ->
            List.map state (\  prevVec ->
                mod =
                    List.map2 vec prevVec  (\ a, b  ->
                        operations.mul a b
                        )
                    |> operations.sumLst
                List.map prevVec (\ elem -> operations.mul elem mod )
                )
            |> List.walk vec (\ remainVec, subVec  ->
                List.map2 remainVec subVec  (\ a, b ->
                    operations.sub a b
                    )
            )
            |> ( \ orto ->
                nextVec = List.map orto (\ elem ->
                    operations.div elem (operations.len orto)
                    )
                List.append state nextVec
                )
        )
    |> transpose


qrDecompositionC : MatrixType (ComplexType a) -> Result (MatrixType (ComplexType a), MatrixType (ComplexType a)) Str
qrDecompositionC = \  mat ->
    # again horrible ,why I have to do that??
    # I want to asign opComplex or opFrec here
    op = {
        greater : (\ a, b ->  Num.isGt ((Num.abs a.0) + (Num.abs a.1)) ((Num.abs b.0) + (Num.abs b.1))),
        equal : (\ a, b -> (Num.isApproxEq  a.0 b.0 {rtol :0.0001 ,atol : 0.00001  }) && (Num.isApproxEq  a.1 b.1 {rtol :0.0001 ,atol : 0.00001  })),
        isZero : (\ a -> ( Num.isZero  a.0 ) && ( Num.isZero  a.1 )),
        sub : Complex.sub,
        mul : Complex.mul,
        div : Complex.div,
        neg : Complex.neg,
        sum : Complex.add,
        sumLst : (\ lst -> List.walk lst (0.0,0.0)
            (\ state, elem->
                (state.0 + elem.0, state.1 + elem.1)  ) ),
        len : (\ lst ->
            len =
                List.map lst (\ a ->
                    (Complex.mul a (Complex.conj a )).0
                    |> Num.abs )
                |> List.sum
                |> Num.sqrt
            (len, 0)
            ),
    }
    q = qInternal mat op
    when inverseC  q is
        Ok invQ ->
            when mulC invQ mat  is
                Ok r ->
                    Ok  ( q, r )
                Err message -> Err message
        Err message -> Err message

isUppTriangMatrix : MatrixType a, OperationType a, a -> Bool
isUppTriangMatrix = \  mat, operations, zero ->
    size = getSize (Matrix.transpose mat)
    if size.0 < 2 || size.1 < 2 then
        Bool.false
    else
        List.walk (Matrix.transpose mat) (Bool.true, 1) (\ triang, col ->
            isZero =
                List.takeLast col (size.0 - triang.1)
                |> List.walk triang.0 ( \ flag, val  ->
                    flag && (operations.equal val zero) )
            (isZero, triang.1 + 1)
        )
        |> (\ result ->
            result.0)

diagonal : MatrixType a -> Result (List a) Str
diagonal = \ mat ->
    List.walkTry mat ([],0) (\ diag, row  ->
        when List.get row  diag.1 is
            Ok elem ->
                Ok (List.append diag.0 elem, diag.1 + 1)
            Err _ -> Err "matrix is not square"
        )
    |> (\ diagResult ->
        when diagResult is
            Ok diag -> Ok diag.0
            Err message -> Err message )

findEigenValues : MatrixType (Frac a), Nat ->  Result (List (ComplexType a))  Str
findEigenValues = \ mat, iter ->
    complexMat =
        List.map mat ( \ row ->
            List.map row (\ elem -> (elem, Num.toFrac 0 ) ) )
    findEigenValuesC complexMat iter

# add shift for oscillating cases
# detect and read complex values
findEigenValuesC : MatrixType (ComplexType a), Nat ->  Result (List (ComplexType a))  Str
findEigenValuesC = \ mat, iter ->
    # again horrible ,why I have to do that??
    # I want to asign opComplex or opFrec here
    op = {
        greater : (\ a, b ->  Num.isGt ((Num.abs a.0) + (Num.abs a.1)) ((Num.abs b.0) + (Num.abs b.1))),
        equal : (\ a, b -> (Num.isApproxEq  a.0 b.0 {rtol :0.0001 ,atol : 0.00001  }) && (Num.isApproxEq  a.1 b.1 {rtol :0.0001 ,atol : 0.00001  })),
        isZero : (\ a -> ( Num.isZero  a.0 ) && ( Num.isZero  a.1 )),
        sub : Complex.sub,
        mul : Complex.mul,
        div : Complex.div,
        neg : Complex.neg,
        sum : Complex.add,
        sumLst : (\ lst -> List.walk lst (0.0,0.0)
            (\ state, elem->
                (state.0 + elem.0, state.1 + elem.1)  ) ),
        len : (\ lst ->
            len =
                List.map lst (\ a ->
                    (Complex.mul a (Complex.conj a )).0
                    |> Num.abs )
                |> List.sum
                |> Num.sqrt
            (len, 0)
            ),
    }

    if iter == 0 then
        diagonal mat
    else
        when qrDecompositionC mat  is
            Ok ( q, r) ->
                when mulC r q  is
                    Ok matPrim ->
                        if isUppTriangMatrix matPrim op (0,0) then
                            diagonal matPrim
                        else
                            findEigenValuesC matPrim (iter - 1)
                    Err message -> Err message
            Err message -> Err message

powerMethodC : MatrixType (ComplexType a),List (ComplexType a), Nat -> Result (List (MatrixType (ComplexType a)))  Str
powerMethodC = \ mat, eigVal, iter ->
    op = {
        greater : (\ a, b ->  Num.isGt ((Num.abs a.0) + (Num.abs a.1)) ((Num.abs b.0) + (Num.abs b.1))),
        equal : (\ a, b -> (Num.isApproxEq  a.0 b.0 {rtol :0.0001 ,atol : 0.00001  }) && (Num.isApproxEq  a.1 b.1 {rtol :0.0001 ,atol : 0.00001  })),
        isZero : (\ a -> ( Num.isZero  a.0 ) && ( Num.isZero  a.1 )),
        sub : Complex.sub,
        mul : Complex.mul,
        div : Complex.div,
        neg : Complex.neg,
        sum : Complex.add,
        sumLst : (\ lst -> List.walk lst (0.0,0.0)
            (\ state, elem->
                (state.0 + elem.0, state.1 + elem.1)  ) ),
        len : (\ lst ->
            len =
                List.map lst (\ a ->
                    (Complex.mul a (Complex.conj a )).0
                    |> Num.abs )
                |> List.sum
                |> Num.sqrt
            (len, 0)
            ),
    }
    powerMethodInternal mat eigVal op iter (Num.toFrac 0, Num.toFrac 0) (Num.toFrac 1, Num.toFrac 0)

powerMethodInternal : MatrixType a, List a, OperationType a, Nat, a, a -> Result (List (MatrixType a))  Str
powerMethodInternal = \ mat, eigVal, op, iter, zero, one ->
    iteration : MatrixType a, MatrixType a, OperationType a, Nat, a -> Result (MatrixType a) Str
    iteration = \ matMod, curVec, inOp, curIter, lastLen ->
        dbg curIter
        if curIter == 0 then
            Ok curVec
        else
            newVecResult = mulInternal matMod curVec inOp

            when newVecResult is
                Ok newVec ->
                    when transpose newVec is
                        [vec] ->
                            den = inOp.len vec
                            iterVec = scalarOp newVec den op.div
                            iteration matMod iterVec inOp (curIter - 1) (inOp.len vec)
                        _ -> Err "some unknown size error"
                Err message -> Err message

    size = getSize mat

    List.walkTry eigVal [] (\ state, eig ->
        unitEigen = scalarOp (unitInternal size.0 zero one ) eig op.mul

        modifiedResult =
            when elemWiseOp mat unitEigen op.sub is
                Ok nomMat ->  inverseInternal nomMat zero one op
                Err message -> Err message

        when modifiedResult is
            Ok modified ->
                when iteration modified (transpose [List.repeat one size.0]) op iter (op.len (List.repeat one size.0)) is
                    Ok eigVec ->
                        Ok (List.append state (transpose eigVec))
                    Err message -> Err message
            Err message -> Err message
        )