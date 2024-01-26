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
    _ <- Stdout.line (Matrix.printMatrix  [[22.0f64, 4.0f64],[2.0f64, 405.55f64],[2.0333f64, 4.0f64]]) |> Task.await
    Stdout.line "test  ok"





