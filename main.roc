app "sym"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.3.1/97mY3sUwo433-pcnEQUlMhn-sWiIf_J9bPhcAFZoqY4.tar.br" }
    imports [
        pf.Process,
        pf.Stdout,
        pf.Stderr,
        pf.Task.{ Task },
        pf.File,
        pf.Path,
        pf.Env,
        Squares,
        Sim,
        pf.Dir
        ]
    provides [main] to pf
 


steps  =4000
runWorld  = \ iter, worldIn ->
    if iter == 0 then
        worldIn
    else
        runWorld (iter - 1 )  (Squares.calcWorldFlow  worldIn)  
        

main =

    path = Path.fromStr "out.txt"
    pathScript = Path.fromStr "script.txt"
    task =
        #_ <- Stdout.line "Writing a string to out.txt" |> Task.await

        #contents <- File.readUtf8 pathScript |> Task.await
        #notify = Squares.runWorld  1000 { blues: [{blue : 0.0},{blue : 1.0},{blue : 0.0}], oranges : [{orange : 0}, {orange : 0} ] }  ""
        x = 300
        y = 300
        z = 300
        modCub  =
            Sim.makeCube x y z 0 
            |> Sim.modifyFieldCube  20 20  20  3
        sol  = (Sim.calculateSolution  (Sim.makeCube x y z 0  ) modCub  0.1 {plus : 0, minus : 0 } 100 )

        log = Sim.makeStringSq (Sim.sliceCube sol {x: All, y :Idx 20, z: All}) 

        _ <- File.writeUtf8 path log |> Task.await
        Stdout.line "done"
        #Stdout.line "I read the file back. Its contents: \"\(contents)\""

    Task.attempt task \result ->
        when result is
            Ok {} -> Stdout.line "Successfully wrote a string to out.txt"
            Err err ->
                msg =
                    when err is
                        FileWriteErr _ PermissionDenied -> "PermissionDenied"
                        FileWriteErr _ Unsupported -> "Unsupported"
                        FileWriteErr _ (Unrecognized _ other) -> other
                        FileReadErr _ _ -> "Error reading file"
                        _ -> "Uh oh, there was an error!"

                {} <- Stderr.line msg |> Task.await
                Process.exit 1