app "hello"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.8.1/x8URkvfyi9I0QhmVG98roKBUs_AZRkLFwFJVJ3942YA.tar.br" }
    imports [
        pf.Stdout,
        pf.Task.{ Task ,await },
        pf.File,
        pf.Path
        ]
    provides [main] to pf


# Performance isn't up to snuff here.
# C++ implementation barely makes it, and there's not much room to play.
# Trying to make this better might not do much. Starts to make you question if this language is up for the heavy stuff.
# I tried to play with the function in other place, it can be done faster but still 100s times slower than C++.
# So, it looks like Roc might not be the best fit for this job as things stand.
# maybe I will use it for scripting
differentialInternal : List (Frac a), List (Frac a), List (Frac a), List (Frac a) -> Result (List (Frac a)) Str
differentialInternal  = \ y, u, yParam, uParam ->
    if List.len y < List.len yParam then
        Err "incorrect input sizes"
    else if List.len u < List.len uParam then
        Ok y
    else
        yPart =
            List.map2  yParam y ( \ param, val -> param * val )
            |> List.sum
        uPart =
            List.map2 (List.reverse uParam) u ( \ param, val -> param * val )
            |> List.sum
        updatedY = uPart - yPart
        differentialInternal (List.prepend y updatedY) (List.dropFirst u 1) yParam uParam



toPrint : List (Num a), List (Num a)  -> Str
toPrint = \ x, y ->
    List.map2 x y  (\ xVal, yVal ->
                        Str.concat (Num.toStr xVal) " "
                        |> Str.concat (Num.toStr yVal) )
    |> Str.joinWith "\n"

yPar = [1f32, -4.1281098f32, 8.5814271f32, -11.3297522f32, 10.3637323f32, -6.7906414f32, 3.2059457f32, -1.0712836f32, 0.2415429f32, -0.0330786f32, 0.0020841f32]
uPar = [0.000040885f32, 0.000408853f32, 0.001839838f32, 0.004906235f32, 0.008585912f32, 0.010303094f32, 0.008585912f32, 0.004906235f32, 0.001839838f32, 0.000408853f32, 0.000040885f32]


main =
    sampSec = 1000000f32
    freq1 = 2f32
    freq2 = 120f32

    xAxis =
        List.map
            (List.range { start: At 0f32, end: At (sampSec - 1) })
            (\ idx -> idx /sampSec)

    u =
        List.map
            xAxis
            (\ val -> Num.sin  (2 * Num.pi *freq1 * val ) +Num.sin  (2 * Num.pi *freq2 * val ) )

    firstNotNeeded = List.dropFirst yPar 1
    y = List.repeat 0 (List.len firstNotNeeded)

    when differentialInternal y u firstNotNeeded uPar is
        Ok lstRev ->
            lst = List.reverse lstRev
            outFilename = Path.fromStr "data.txt"
            #_ <- File.writeUtf8 outFilename (toPrint xAxis lst) |> Task.attempt
            Stdout.line "processing  ok!"
        Err str  -> Stdout.line str
