app "peek"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [
        pf.Stdin,
        pf.Stdout,
        pf.Task.{ Task ,await },
        Matrix,
        ]
    provides [main] to pf

main =



    aa = Matrix.create [[2 ,3, 2, 1 , 7], [4 ,5 ,-1,2 ,4 ],[ 1 , 1 ,3,3, 9 ],[ 1 , 1 ,-1 ,0, 2 ], [ 1,-1,-2,3,4 ]] Num.toF64

    bb = Matrix.create [[1, 1, 3, 2, -1]]  Num.toF64


    multi = \ a, b ->
        when (a, b) is
            (Ok aMat, Ok bMat) ->
                dbg   aMat
                dbg bMat
                when  Matrix.solve  aMat bMat  is
                    Ok result ->
                        when Matrix.mul aMat (Matrix.transpose  result) is
                            Ok backToResult ->
                                (Matrix.printMatrix backToResult)
                            Err message -> message
                    Err  message -> message
            _ -> "at least  one matrix incorrectly created"


    _ <- Stdout.line (multi  aa  bb)  |> Task.await
    Stdout.line "test  ok"





