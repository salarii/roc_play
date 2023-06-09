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
        Util,
        pf.Dir
        ]
    provides [main] to pf
 


steps  =4000
runWorld  = \ iter, worldIn ->
    if iter == 0 then
        worldIn
    else
        runWorld (iter - 1 )  (Squares.calcWorldFlow  worldIn)  
        
force = \ list, deltaT, cnt ->
    (List.replace  list   50  (Util.createNode   (Num.sin ((3.14/(20.0/deltaT) ) * ( Num.toF32  cnt ))) 1 ) ).list

getRelevant = \ elem ->
    elem.value
    
main =

    path = Path.fromStr "out.txt"
    pathScript = Path.fromStr "script.txt"
    task =
        #_ <- Stdout.line "Writing a string to out.txt" |> Task.await

        #contents <- File.readUtf8 pathScript |> Task.await
        #notify = Squares.runWorld  1000 { blues: [{blue : 0.0},{blue : 1.0},{blue : 0.0}], oranges : [{orange : 0}, {orange : 0} ] }  ""
        orange = List.repeat (Util.createNode   0 1) 200
        blue = List.repeat (Util.createNode   0 1) 201
        
        orangeCalc = Sim.lineMotion orange  blue force {front : (Util.createNode   0 1), back : (Util.createNode   0 1) } 12000 []
        
        log = Sim.makeStringSq orangeCalc getRelevant "\n"

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