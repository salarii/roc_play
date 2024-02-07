app "peek"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [
        pf.Stdin,
        pf.Stdout,
        pf.Task.{ Task ,await },
        pf.File,
        pf.Path,
        Matrix,
        Solvers,
        Complex,
        ]
    provides [main] to pf


# gg = \ x, mat ->
#     when mat is
#         [lst] ->
#             when lst is
#                 [y1, y2] ->
#                     y2
#                 _-> 0
#         _ -> 0

# ff = \ x, mat ->
#     when mat is
#         [lst] ->
#             when lst is
#                 [y1, y2] ->
#                     -( Num.sin y1) * y2
#                 _-> 0
#         _ -> 0

# f = \ mat ->
#     when mat is
#         [lst] ->
#             when lst is
#                 [x,r] ->
#                     -r * x + Num.sin x
#                 _-> 0
#         _ -> 0

# g = \ mat ->
#     when mat is
#         [lst] ->
#             when lst is
#                 [x,r] ->
#                     r - Num.cos x
#                 _-> 0
#         _ -> 0

main =
    # dbg Complex.print ( Complex.div (1.1f64,-1f64)  (3.2f64, 3.9f64) )
    # outFilename = Path.fromStr "data.txt"
    # pp = Solvers.rkSolver  -1f64  0f64  0.01f64 0.2f64  0.01f64  10f64  ( \ a, b -> Num.sin a )  []
    # str =
    #     List.map pp  ( \val ->
    #         Str.concat (Num.toStr  val.0) " "
    #         |> Str.concat (Num.toStr  val.1))
    #     |> Str.joinWith "\n"

    # hh  = Solvers.rkSolverM  [[0f64,1f64]] 0f64  0.01f64 0.1f64  0.01f64  8f64 [gg, ff]  []
    # str2 =
    #     when  hh is
    #         Ok h ->
    #             List.map h (\ val ->
    #                 when val.1 is
    #                     [[y1, y2]] ->
    #                         Str.concat (Num.toStr  val.0) " "
    #                         |> Str.concat (Num.toStr  y1)
    #                         |> Str.concat " "
    #                         |> Str.concat (Num.toStr  y2)
    #                     _ -> "error" )
    #             |> Str.joinWith "\n"

    #         Err message -> message
    # dbg  pp
    # hhh =  Solvers.tryFindZeroPoint [f, g] [[3.14f64 * 3f64/4f64, 0.25f64]]   20  0.01
    # out = when hhh is
    #             Ok tada ->
    #                 (Matrix.print tada)
    #             Err message -> message

    matiRes  = Matrix.create [[(3,0), (4,0), (1,0)],[ (-4,0), (1,0), (8,0)],[(-1,0)  , (-3,0), (5,0)]] (\ a-> (Num.toF64 a.0, Num.toF64 a.1))
    ggg =
        when matiRes is
            Ok  mati ->
                when Matrix.solveC mati  [[(1f64,0f64)  , (2f64,0f64),  (0f64,7f64)]] is
                        Ok h ->
                            Matrix.printC h

                        Err message -> message
            Err message -> message

    # gg =
    #     when (Matrix.create [[1, 0, 1, 0],[ 3, 3, 7, 1 ],[ -1,2,2, 0],[ -2,1,1, 0]] Num.toF64) is
    #         Ok tada ->
    #             when Matrix.inverse  tada  is
    #                 Ok inv ->
    #                     dbg inv
    #                     (Matrix.print inv)
    #                 Err message -> message
    #         Err message -> message

    #_ <- Stdout.line (Matrix.print (Matrix.unit 20 )) |> Task.await
    # _ <- Stdout.line out  |> Task.await
    # _ <- File.writeUtf8 outFilename str2 |> Task.attempt
    # _ <- Stdout.line str |> Task.await
    # _ <- Stdout.line "\n\n\n" |> Task.await
    _ <- Stdout.line ggg |> Task.await
    Stdout.line "test  ok"





