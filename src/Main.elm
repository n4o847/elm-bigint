module Main exposing (main)

import BigInt exposing (..)
import Html exposing (text)


main =
    text <|
        "Hello "
            ++ (if Just zero == BigInt.fromString "-0" then
                    "true"

                else
                    "false"
               )
            ++ (BigInt.toString <|
                    BigInt.add
                        (Maybe.withDefault zero <| BigInt.fromString "999999999999999999999999999999999999999999999999999999999999")
                        (Maybe.withDefault zero <| BigInt.fromString "9999")
               )
