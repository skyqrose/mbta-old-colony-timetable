module Helpers exposing (uniq)

{-|

    uniq [ 1, 3, 2, 1, 2 ]
    --> [1, 3, 2]

    Result is ordered by first appearance.

-}


uniq : List a -> List a
uniq a =
    a
        |> List.foldl
            (\new acc ->
                if List.member new acc then
                    acc

                else
                    new :: acc
            )
            []
        |> List.reverse
