        app "exp"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.3.1/97mY3sUwo433-pcnEQUlMhn-sWiIf_J9bPhcAFZoqY4.tar.br" }
    imports [pf.Stdout, Parsing] 
    provides [main] to pf



main =
    #p  = Num.toDec ( Str.toF32  "50.4"  )
    eton =  Parsing.setupEnv  "nodes 50  \n orange  0  \n  blue  0.3  \n   "
    dbg eton 
    Stdout.line "I'm a Roc application!"