interface Matrix
    exposes [ printMatrix, solve]
    imports []

MatrixType a : List ( List (Frac a))

getMatrixValue : MatrixType a, Nat, Nat -> Result (Frac a) Str
getMatrixValue = \ mat, row, col ->
    when List.get mat row is
        Ok rowLst ->
            when List.get rowLst col is
                Ok val -> Ok val
                _ -> Err "wrong col index"
        _ -> Err "wrong row index"

gaussMethod : MatrixType a -> Result (MatrixType a) Str
gaussMethod = \ mat ->
    gaussMethodInternal mat []

gaussMethodInternal : MatrixType a, MatrixType a -> Result (MatrixType a) Str
gaussMethodInternal = \ mat, tmp ->
    sorter : List (Frac a), List (Frac a) -> [ LT, EQ, GT ]
    sorter = \ left, right ->
        when (List.get left line, List.get right line) is
            (Ok leftElem, Ok  rightElem) ->
                if Num.isGt rightElem leftElem  then
                    GT
                else if Num.isApproxEq rightElem leftElem  {rtol :0.00001 } then
                    EQ
                else
                    LT
            _ -> GT

    line = List.len tmp
    sortedMat = List.sortWith mat sorter
    when sortedMat is
        [] ->
            Ok tmp
        [head ,.. as tail] ->
                List.walkTry tail [] (\ state, arr ->
                    when (List.get head line, List.get arr line) is
                            (Ok active, Ok pasive ) ->
                                if Num.isZero active then
                                    Err "operation can't be performed on this matrix"
                                else
                                    modifier = pasive/active
                                    when arr is
                                        [.. as lst, idx] ->
                                            Ok (List.append state (
                                                (List.map2 lst head ( \ pasiveElem, activeElem ->
                                                        pasiveElem - modifier * activeElem
                                                    ))
                                                |> List.append idx))
                                        _ -> Err "operation can't be performed on this matrix"
                            _ -> Err "operation can't be performed on this matrix")
                    |> ( \ processingResult ->
                        when processingResult is
                            Ok processed ->
                                gaussMethodInternal processed (List.append tmp head)
                            Err message -> Err message
                    )

solveFromGauss : MatrixType a, MatrixType a -> Result (MatrixType a) Str
solveFromGauss = \ b, gauss ->
    last = List.len (List.join b)
    List.walkTry (List.reverse  (List.join b)) [] ( \ state, bi ->
        idx = (last - 1 - (List.len state))
        when List.get gauss idx is
            Ok row ->
                when  List.get row (idx) is
                    Ok aii ->
                        List.map2 state (List.reverse row) ( \ xi, ai  ->
                            -xi * ai
                        )
                        |> List.sum
                        |> ( \ other ->
                            Ok (List.append  state ((bi + other)/aii))
                        )
                    _ -> Err "unexpected size problem"

            _ -> Err "not compatible sizes"
    )
    |> (\ unboxed ->
        when unboxed is
            Ok unbox -> Ok [unbox]
            Err message -> Err message)

getSize : MatrixType a -> (Nat, Nat)
getSize = \ mat ->
    when mat is
        [] -> (0,0)
        [head, .. as tail] ->
            ( List.len mat, List.len head )

solve : MatrixType a, MatrixType a -> Result (MatrixType a) Str
solve = \  a, b ->
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

        idx =
            List.range { start: At 0, end: At (sizeA.0 - 1) }
            |> List.map ( \ val -> Num.toFrac val )

        when merge a updatedB 1 is
            Ok glued ->
                when merge glued (transpose [idx]) 1 is
                    Ok gluedWithIdx ->
                        dbg  glued
                        dbg  gluedWithIdx

                        when gaussMethod gluedWithIdx is
                            Ok gauss ->
                                dbg gauss
                                when split gauss 1 (sizeA.0 +1 ) is
                                    Ok  splitOnIdx ->
                                        when split splitOnIdx.0 1 sizeA.0 is
                                            Ok splited ->
                                                when solveFromGauss splited.1 splited.0 is
                                                    Ok solution ->
                                                        List.walkTry (List.join splitOnIdx.1)  [] (\ sol, id ->
                                                            when List.get (List.join solution) (Num.toNat (Num.floor id)) is
                                                                Ok result ->
                                                                    Ok (List.append sol result)
                                                                _ -> Err "problem during result processing"
                                                            ) |> (\  finalOut ->
                                                                when finalOut is
                                                                    Ok final -> Ok [final]
                                                                    Err message -> Err message
                                                                )
                                                    Err message -> Err message
                                            Err message -> Err message
                                    Err message -> Err message
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
                List.map2 state  (List.chunksOf head 1) ( \ left, right ->
                    List.concat left right ) )

merge : MatrixType a, MatrixType a, Nat -> Result (MatrixType a) Str
merge = \ dest, src, dim ->
    sizeDest = getSize dest
    sizeSrc = getSize src
    if dim == 0 then
        if sizeDest.1 == sizeSrc.1 then
            Ok (List.concat dest src)
        else
            Err "Incompatible sizes can't split"
    else if dim == 1 then
        if sizeDest.0 == sizeSrc.0 then
            Ok (
                List.map2 dest src ( \ left, right ->
                    List.concat  left right ) )
        else
            Err "Incompatible sizes can't split"

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

printMatrix : MatrixType a -> Str
printMatrix = \ mat ->
    adjustFront : Str, Str, Nat -> Str
    adjustFront = \ str, filler, cnt ->
        Str.concat ( Str.repeat  filler  cnt) str

    strRound : Frac a, Frac a -> Str
    strRound = \ val, round ->

        rounded =
            Num.toStr (Num.floor ( ( val - (Num.toFrac (Num.floor val))) * (Num.pow 10.0 round)))
            |> ( \ restStr ->
                adjustFront restStr "0" ((Num.toNat (Num.floor round)) - (List.len  (Str.toUtf8 restStr)) ))
        #adjustment = ( Str.repeat  " " ( (Num.toNat (Num.floor round)) -  (List.len (Str.toUtf8 rounded )) ) )

        Num.toStr (Num.floor val)
        |> Str.concat "."
        |> Str.concat rounded
        #|> Str.concat adjustment


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

