interface Matrix
    exposes [
        print,
        solve,
        mul,
        transpose,
        elemWiseOp,
        create,
        merge,
        inverse,
        getSize,
        split,
        scalarOp,
        createSingleValue,
        norm,
        unit,
        ]
    imports []

MatrixType a : List ( List (a))

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
        greater : Num.isGt,
        equal : (\ a, b -> Num.isApproxEq  a b {rtol :0.00001 } ),
        isZero : Num.isZero,
        sub : Num.sub ,
        mul : Num.mul,
        div : Num.div,
        abs : Num.abs,
        neg : (\a -> -a),
        sum : (\a, b -> a + b),
        sumLst : (\ lst -> List.sum lst)
    }
    solveInternal aMat bMat op
    # as far as I am concerned this should work
    #solveInternal a b opFrec

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
        greater : Num.isGt,
        equal : (\ a, b -> Num.isApproxEq  a b {rtol :0.00001 } ),
        isZero : Num.isZero,
        sub : Num.sub ,
        mul : Num.mul,
        div : Num.div,
        abs : Num.abs,
        neg : (\a -> -a),
        sum : (\a, b -> a + b),
        sumLst : (\ lst -> List.sum lst)
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
    op = {
        greater : Num.isGt,
        equal : (\ a, b -> Num.isApproxEq  a b {rtol :0.00001 } ),
        isZero : Num.isZero,
        sub : Num.sub ,
        mul : Num.mul,
        div : Num.div,
        abs : Num.abs,
        neg : (\a -> -a),
        sum : (\a, b -> a + b),
        sumLst : (\ lst -> List.sum lst)
    }
    inverseInternal mat (Num.toFrac 0) (Num.toFrac 1) op

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
    abs : (a -> a),
    neg : (a -> a),
    sum : (a, a -> a),
    sumLst : (List a ->  a ),
}

opFrec : OperationType (Frac a)
opFrec = {
    greater : Num.isGt,
    equal : (\ a, b -> Num.isApproxEq  a b {rtol :0.00001 } ),
    isZero : Num.isZero,
    sub : Num.sub ,
    mul : Num.mul,
    div : Num.div,
    abs : Num.abs,
    neg : (\a -> -a),
    sum : (\a, b -> a + b),
    sumLst : (\ lst -> List.sum lst),
}
# #
# # Num.isGt
# #
gaussElimination : MatrixType a, OperationType a -> Result (MatrixType a) Str
gaussElimination = \ mat, op ->
    gaussEliminationInternal mat [] op

gaussEliminationInternal : MatrixType a, MatrixType a, OperationType a -> Result (MatrixType a) Str
gaussEliminationInternal = \ mat, tmp, operations ->

    sorter : List a, List  a, OperationType a -> [ LT, EQ, GT ]
    sorter = \ left, right, op ->
            when (List.get left line, List.get right line) is
                (Ok leftElem, Ok  rightElem) ->
                    if (op.greater (op.abs rightElem) (op.abs leftElem)) then
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