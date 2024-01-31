app "peek"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [
        pf.Stdin,
        pf.Stdout,
        pf.Task.{ Task ,await },
        Matrix,
        Solvers,
        ]
    provides [main] to pf


f = \ mat ->
    when mat is
        [lst] ->
            when lst is
                [x, y] ->
                    (Num.pow  x  2) +  (Num.pow  y  2) - 10
                _-> 0
        _ -> 0

g = \ mat ->
    when mat is
        [lst] ->
            when lst is
                [x, y] ->
                    x * y - 4
                _-> 0
        _ -> 0

main =

    pp = Solvers.rkSolver  -1f64  0f64  0.1f64  0.0001f64  10f64  ( \ a, b -> Num.sin a )  []
    str =
        List.map pp  ( \val ->
            Str.concat (Num.toStr  val.0) "="
            |> Str.concat (Num.toStr  val.1))
        |> Str.joinWith "  "
    # dbg  pp
    hhh =  Solvers.tryFindZeroPoint [f, g] [[4f64, 4f64]]   20  0.01
    out = when hhh is
                Ok tada ->
                    (Matrix.printMatrix tada)
                Err message -> message
    aa = Matrix.create [[2 ,3, 2, 1 , 7], [4 ,5 ,-1,2 ,4 ],[ 1 , 1 ,3,3, 9 ],[ 1 , 1 ,-1 ,0, 2 ], [ 1,-1,-2,3,4 ]] Num.toF64

    # bb = Matrix.create [[1, 1, 3, 2, -1]]  Num.toF64

    # gg =
    #     when (Matrix.create [[1, 0, 1, 0],[ 3, 3, 7, 1 ],[ -1,2,2, 0],[ -2,1,1, 0]] Num.toF64) is
    #         Ok tada ->
    #             when Matrix.inverse  tada  is
    #                 Ok inv ->
    #                     dbg inv
    #                     (Matrix.printMatrix inv)
    #                 Err message -> message
    #         Err message -> message

    #_ <- Stdout.line (Matrix.printMatrix (Matrix.unit 20 )) |> Task.await
    # _ <- Stdout.line out  |> Task.await
    _ <- Stdout.line str |> Task.await
    Stdout.line "test  ok"





