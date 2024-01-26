interface Matrix
    exposes [gaussMethod, printMatrix]
    imports []

MatrixType a : List ( List (Frac a))

gaussMethod : MatrixType a -> MatrixType a
gaussMethod = \ mat ->
    mat

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

