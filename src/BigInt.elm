module BigInt exposing (BigInt, add, compare, fromInt, fromString, one, sub, toString, zero)

-- module BigInt exposing (BigInt, abs, add, compare, fromInt, negate, one, sub, toString, zero)


type BigInt
    = Positive Digits
    | Zero
    | Negative Digits


type alias Digits =
    List Int


baseLength : Int
baseLength =
    7


base : Int
base =
    10 ^ baseLength


head : Digits -> Int
head digits =
    List.head digits |> Maybe.withDefault 0


tail : Digits -> Digits
tail digits =
    List.tail digits |> Maybe.withDefault []


ifThenElse : Bool -> a -> a -> a
ifThenElse a b c =
    if a then
        b

    else
        c


zero : BigInt
zero =
    Zero


one : BigInt
one =
    Positive [ 1 ]


negate : BigInt -> BigInt
negate a =
    case a of
        Positive a_ ->
            Negative a_

        Zero ->
            Zero

        Negative a_ ->
            Positive a_


abs : BigInt -> BigInt
abs a =
    case a of
        Positive a_ ->
            Positive a_

        Zero ->
            Zero

        Negative a_ ->
            Positive a_


compare_ : Digits -> Digits -> Order
compare_ a b =
    let
        ord =
            Basics.compare (List.length a) (List.length b)
    in
    if ord == EQ then
        Basics.compare (List.reverse a) (List.reverse b)

    else
        ord


compare : BigInt -> BigInt -> Order
compare a b =
    case ( a, b ) of
        ( Zero, Zero ) ->
            EQ

        ( Positive a_, Positive b_ ) ->
            compare_ a_ b_

        ( Positive _, _ ) ->
            GT

        ( _, Positive _ ) ->
            LT

        ( Negative a_, Negative b_ ) ->
            compare_ b_ a_

        ( _, Negative _ ) ->
            GT

        ( Negative _, _ ) ->
            LT


add_ : Bool -> Digits -> Digits -> Digits
add_ carry a b =
    case ( carry, a, b ) of
        ( False, _, [] ) ->
            a

        ( False, [], _ ) ->
            b

        ( True, [], [] ) ->
            [ 1 ]

        _ ->
            let
                sum =
                    head a + head b + ifThenElse carry 1 0
            in
            modBy base sum :: add_ (sum >= base) (tail a) (tail b)


sub_ : Bool -> Digits -> Digits -> Digits
sub_ borrow a b =
    case ( borrow, a, b ) of
        ( False, _, [] ) ->
            a

        ( False, [], _ ) ->
            -- never happens
            []

        ( True, [], [] ) ->
            -- never happens
            []

        _ ->
            let
                sum =
                    head a - head b - ifThenElse borrow 1 0
            in
            modBy base sum :: sub_ (sum < 0) (tail a) (tail b)


add : BigInt -> BigInt -> BigInt
add a b =
    case ( a, b ) of
        ( _, Zero ) ->
            a

        ( Zero, _ ) ->
            b

        ( Positive a_, Positive b_ ) ->
            Positive (add_ False a_ b_)

        ( Positive a_, Negative b_ ) ->
            sub (Positive a_) (Positive b_)

        ( Negative a_, Positive b_ ) ->
            sub (Positive b_) (Positive a_)

        ( Negative a_, Negative b_ ) ->
            Negative (add_ False a_ b_)


sub : BigInt -> BigInt -> BigInt
sub a b =
    case ( a, b ) of
        ( _, Zero ) ->
            a

        ( Zero, _ ) ->
            negate b

        ( Positive a_, Positive b_ ) ->
            case compare_ a_ b_ of
                EQ ->
                    Zero

                LT ->
                    Negative (sub_ False b_ a_)

                GT ->
                    Positive (sub_ False a_ b_)

        ( Positive a_, Negative b_ ) ->
            add (Positive a_) (Positive b_)

        ( Negative a_, Positive b_ ) ->
            add (Negative a_) (Negative b_)

        ( Negative a_, Negative b_ ) ->
            sub (Positive b_) (Positive a_)


fromInt_ : Int -> Digits
fromInt_ i =
    if i == 0 then
        []

    else
        modBy base i :: fromInt_ (i // base)


fromInt : Int -> BigInt
fromInt i =
    case Basics.compare i 0 of
        LT ->
            Negative (fromInt_ (Basics.abs i))

        EQ ->
            Zero

        GT ->
            Positive (fromInt_ i)


fromStringUnsigned : String -> Maybe Digits
fromStringUnsigned s =
    if String.all Char.isDigit s then
        let
            fromString_ s_ =
                if s_ == "" then
                    []

                else if String.all ((==) '0') s_ then
                    []

                else
                    Maybe.withDefault 0 (String.toInt (String.right 7 s_)) :: fromString_ (String.dropRight 7 s_)
        in
        Just (fromString_ s)

    else
        Nothing


fromString : String -> Maybe BigInt
fromString s =
    case String.uncons s of
        Just ( h, s_ ) ->
            if h == '-' then
                fromStringUnsigned s_
                    |> Maybe.map Negative

            else
                fromStringUnsigned s
                    |> Maybe.map Positive

        Nothing ->
            Nothing


toString_ : Digits -> String
toString_ a =
    case a of
        [] ->
            ""

        [ d ] ->
            String.fromInt d

        d :: dd ->
            toString_ dd ++ String.padLeft 7 '0' (String.fromInt d)


toString : BigInt -> String
toString a =
    case a of
        Zero ->
            "0"

        Positive a_ ->
            toString_ a_

        Negative a_ ->
            "-" ++ toString_ a_
