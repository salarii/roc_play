app "sym"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br" }
    imports [
        pf.Stdout,
        pf.Stderr,
        pf.Task.{ Task },
        pf.File,
        pf.Path,
        pf.Env,
        pf.InternalTask, 
        pf.Effect,
        Squares,
        Sim,
        Util,
        pf.Dir]
    provides [main] to pf
 
    
force = \ list, deltaT, cnt ->
    (List.replace  list  10  (Util.createNode   1 1 0)).list


forceSq = \ sq, deltaT, cnt ->
    Sim.modifyFieldSq  sq  2 2   (Util.createNode  1 1 0)

forceSq2 = \ sq, deltaT, cnt ->
    Sim.modifyFieldSq  sq  9 9  (Util.createNodeAni  1 1 0 0 0)

    
forceCube = \ cubeX1, cubeY1, cubeZ1, cubeX2, cubeY2, cubeZ2, deltaT, cnt ->
    modifZ2  = Sim.modifyFieldCube  cubeZ2  1 1 1  (Util.createNode  1 1 0)
    {xField1 : cubeX1,  yField1 : cubeY1, zField1  : cubeZ1, xField2 : cubeX2,  yField2 : cubeY2, zField2  : modifZ2 } 

forceCube2 = \ cubeX1, cubeY1, cubeZ1, cubeX2, cubeY2, cubeZ2, deltaT, cnt ->
    modifZ2  = Sim.modifyFieldCube  cubeZ2  1 1 1   (Util.createNodeAni  1 1 0 0 0)
    {xField1 : cubeX1,  yField1 : cubeY1, zField1  : cubeZ1, xField2 : cubeX2,  yField2 : cubeY2, zField2  : modifZ2 } 

    

    
main =
    task =
        # 1D  simulation
        #path = Path.fromStr "out.txt"
        #orange = List.repeat (Util.createNode   0 1  2.5) 200
        #blue = List.repeat (Util.createNode   0 1  0) 201
        #orangeCalc = Sim.lineMotion orange  blue force {front : (Util.createNode   0 1 0 ), back : (Util.createNode   0 1 0) } 12000 []
        #log = Sim.makeStringSq orangeCalc getRelevant "\n"
        #_ <- File.writeUtf8 path log |> Task.await
        #zFieldPath = Path.fromStr "zField.txt"
        #xFieldPath = Path.fromStr "xField.txt"
        #yFieldPath = Path.fromStr "yField.txt"
        #size  = 80
        # 2D simulation
        #xOrange = Sim.makeSquare  size   (size +1)   (Util.createNode   0 1 0)
        #yOrange = Sim.makeSquare  (size+1)   size  (Util.createNode   0 1 0)
        #zBlue  = Sim.makeSquare  size   size   (Util.createNode   0 1 0.1)
        #result = Sim.xyVariationSim  xOrange yOrange zBlue forceSq 1000  {zField  : [], xField : [],  yField : []}        
        # 2D simulation 2
        #xOrange = Sim.makeSquare  size   (size + 1)   (Util.createNodeAni   0 1 0 0 0)
        #yOrange = Sim.makeSquare  (size + 1)   size   (Util.createNodeAni   0 1 0 0 0)
        #zBlue  = Sim.makeSquare  size   size   (Util.createNodeAni   0 1 0 0 0)
        
        #xOrangePml = Sim.pmlIzeSq xOrange 10  1 1
        #yOrangePml = Sim.pmlIzeSq yOrange 10  1 1
        #zBluePml = Sim.pmlIzeSq zBlue 10  1 1
        
        #result = Sim.xyVariationSim2  {deltaSpace : 1, deltaT : 0.1}  xOrangePml yOrangePml zBluePml forceSq2 200 {zField  : [], xField : [],  yField : []}
      

        #zlog = ( Sim.makeStringCube result.zField getRelevant  {y:"\n",z:""} )
        #xlog = ( Sim.makeStringCube result.xField getRelevant  {y:"\n",z:""} )
        #ylog = ( Sim.makeStringCube result.yField getRelevant  {y:"\n",z:""} )
        #_ <- File.writeUtf8 zFieldPath zlog |> Task.await
        #_ <- File.writeUtf8 xFieldPath xlog |> Task.await
        #_ <- File.writeUtf8 yFieldPath ylog |> Task.await
        #Stdout.line "done"
        
        
        #  3D  simulation  1
        xField1Path = Path.fromStr "xField1.txt"
        yField1Path = Path.fromStr "yField1.txt"
        zField1Path = Path.fromStr "zField1.txt"
        xField2Path = Path.fromStr "xField2.txt"
        yField2Path = Path.fromStr "yField2.txt"
        zField2Path = Path.fromStr "zField2.txt"
                
        #size  = 3
        #xOrange = Sim.makeCube size (size + 1) (size + 1) (Util.createNode   0 1 0)
        #yOrange = Sim.makeCube (size + 1) size (size + 1) (Util.createNode   0 1 0)
        #zOrange = Sim.makeCube (size + 1) (size + 1) size (Util.createNode   0 1 0)
        
        #xBlue = Sim.makeCube (size +1 ) size size (Util.createNode   0 1 0)
        #yBlue = Sim.makeCube size (size + 1) size (Util.createNode   0 1 0)
        #zBlue = Sim.makeCube size size (size + 1) (Util.createNode   0 1 0)

        #result = Sim.xyzVariationSim  {deltaSpace : 1, deltaT : 0.1} xOrange yOrange zOrange xBlue yBlue zBlue forceCube 2 {xField1 : [],  yField1 : [], zField1  : [], xField2 : [],  yField2 : [], zField2  : [] }  
    

        #xF1log = (Util.printCubes  result.xField1  {y:"\n",z:""})
        #yF1log = (Util.printCubes  result.yField1  {y:"\n",z:""})
        #zF1log = (Util.printCubes  result.zField1  {y:"\n",z:""})
        #xF2log = (Util.printCubes  result.xField2  {y:"\n",z:""})
        #yF2log = (Util.printCubes  result.yField2  {y:"\n",z:""})
        #zF2log = (Util.printCubes  result.zField2  {y:"\n",z:""})  

        #_ <- File.writeUtf8 xField1Path xF1log |> Task.await
        #_ <- File.writeUtf8 yField1Path yF1log |> Task.await
        #_ <- File.writeUtf8 zField1Path zF1log |> Task.await        
        #_ <- File.writeUtf8 xField2Path xF2log |> Task.await
        #_ <- File.writeUtf8 yField2Path yF2log |> Task.await
        #_ <- File.writeUtf8 zField2Path zF2log |> Task.await          
        #Stdout.line (Util.printCubes  result.zField2  {y:"\n",z:"\n"})
        
        #  3D  simulation  2
        size  = 3
        xOrange = Sim.makeCube size (size + 1) (size + 1) (Util.createNodeAni   0 1 0 0 0)
        yOrange = Sim.makeCube (size + 1) size (size + 1) (Util.createNodeAni   0 1 0 0 0)
        zOrange = Sim.makeCube (size + 1) (size + 1) size (Util.createNodeAni   0 1 0 0 0)
        
        xBlue = Sim.makeCube (size +1 ) size size (Util.createNodeAni   0 1 0 0 0)
        yBlue = Sim.makeCube size (size + 1) size (Util.createNodeAni   0 1 0 0 0)
        zBlue = Sim.makeCube size size (size + 1) (Util.createNodeAni   0 1 0 0 0)

        result = Sim.xyzVariationSim2  {deltaSpace : 1, deltaT : 0.1} xOrange yOrange zOrange xBlue yBlue zBlue forceCube2 1000 {xField1 : [],  yField1 : [], zField1  : [], xField2 : [],  yField2 : [], zField2  : [] }  
    
        xF1log = (Util.printCubes  result.xField1  {y:"\n",z:""})
        yF1log = (Util.printCubes  result.yField1  {y:"\n",z:""})
        zF1log = (Util.printCubes  result.zField1  {y:"\n",z:""})
        xF2log = (Util.printCubes  result.xField2  {y:"\n",z:""})
        yF2log = (Util.printCubes  result.yField2  {y:"\n",z:""})
        zF2log = (Util.printCubes  result.zField2  {y:"\n",z:""})  

        _ <- File.writeUtf8 xField1Path xF1log |> Task.await
        _ <- File.writeUtf8 yField1Path yF1log |> Task.await
        _ <- File.writeUtf8 zField1Path zF1log |> Task.await        
        _ <- File.writeUtf8 xField2Path xF2log |> Task.await
        _ <- File.writeUtf8 yField2Path yF2log |> Task.await
        _ <- File.writeUtf8 zField2Path zF2log |> Task.await          
        Stdout.line (Util.printCubes  result.yField2  {y:"\n",z:"\n"})
        
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
                Task.ok {}