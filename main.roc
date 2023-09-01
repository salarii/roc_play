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
    (List.replace  list   50  (Util.createNode   (Num.sin ((3.14/(100.0/deltaT) ) * ( Num.toF32  cnt ))) 1  0) ).list

forceSq = \ sq, deltaT, cnt ->
    sq


getRelevant = \ elem ->
    elem.value
    
main =
    task =
        # 1D  simulation
        #path = Path.fromStr "out.txt"
        #orange = List.repeat (Util.createNode   0 1  2.5) 200
        #blue = List.repeat (Util.createNode   0 1  0) 201
        #orangeCalc = Sim.lineMotion orange  blue force {front : (Util.createNode   0 1 0 ), back : (Util.createNode   0 1 0) } 12000 []
        #log = Sim.makeStringSq orangeCalc getRelevant "\n"
        #_ <- File.writeUtf8 path log |> Task.await
        # 2D simulation
        zFieldPath = Path.fromStr "zField.txt"
        xFieldPath = Path.fromStr "xField.txt"
        yFieldPath = Path.fromStr "yField.txt"
        xOrange = Sim.makeSquare  2   3   (Util.createNode   0 5 0)
        yOrange = Sim.makeSquare  3   2   (Util.createNode   0 5 0)
        zBlue  = Sim.makeSquare  2   2   (Util.createNode   0 5 0)
        result = Sim.xyVariationSim  xOrange yOrange zBlue forceSq 3  {zField  : [], xField : [],  yField : []}
        zlog = ( Sim.makeStringCube result.zField getRelevant  {y:"\n",z:""} )
        xlog = ( Sim.makeStringCube result.xField getRelevant  {y:"\n",z:""} )
        ylog = ( Sim.makeStringCube result.yField getRelevant  {y:"\n",z:""} )
        _ <- File.writeUtf8 zFieldPath zlog |> Task.await
        _ <- File.writeUtf8 xFieldPath xlog |> Task.await
        _ <- File.writeUtf8 yFieldPath ylog |> Task.await
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