interface Complex
    exposes [
        add,
        sub,
        mul,
        div,
        conj,
        print,
        ComplexType]
    imports []

ComplexType a : (Frac a, Frac a)

print : ComplexType a -> Str
print = \  a ->
    adjustFront : Str, Str, Nat -> Str
    adjustFront = \ str, filler, cnt ->
        Str.concat ( Str.repeat  filler  cnt) str

    strRound : Frac a, Frac a -> Str
    strRound = \ val, round ->

        rounded =
            Num.toStr (Num.floor ( ( ( (Num.abs val) - (Num.toFrac (Num.floor (Num.abs val))))) * (Num.pow 10.0 round)))
            |> ( \ restStr ->
                adjustFront restStr "0" ((Num.toNat (Num.round round)) - (List.len  (Str.toUtf8 restStr)) ))

        valueStr =
            if Num.isNegative val then
                str = Num.toStr (Num.ceiling val)
                if (Num.ceiling val) == 0 then
                    Str.concat "-" str
                else
                    str
            else
                Num.toStr (Num.floor val)

        valueStr
        |> Str.concat "."
        |> Str.concat rounded

    (strRound  a.0  3)
    |> Str.concat "  "
    |> Str.concat (strRound  a.1  2)
    |> Str.concat "i"

add : ComplexType a, ComplexType a -> ComplexType a
add = \ a, b ->
    (a.0 + b.0, a.1 + b.1)

sub : ComplexType a, ComplexType a -> ComplexType a
sub = \ a, b ->
    (a.0 - b.0, a.1 - b.1)

mul : ComplexType a, ComplexType a -> ComplexType a
mul = \ a, b ->
    (a.0 * b.0  - (a.1 * b.1), a.1 * b.0 + a.0 * b.1)

conj : ComplexType a -> ComplexType a
conj = \ a ->
    (a.0, -a.1)

neg : ComplexType a -> ComplexType a
neg = \ a ->
    (-a.0, -a.1)

real : ComplexType a -> Frac a
real = \ a ->
    a.0

imag : ComplexType a -> Frac a
imag = \ a ->
    a.1

div : ComplexType a, ComplexType a -> ComplexType a
div = \ a, b ->
    bConj = conj b
    dbg  (real (mul b  bConj))
    dbg  (real (mul a bConj) )
    ( (real (mul a bConj) )/ (real (mul b  bConj)), (imag (mul a bConj) )/ (real (mul b bConj)))

