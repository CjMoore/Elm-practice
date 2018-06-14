module Example exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import ListPermutation as List 


suite : Test
suite =
  describe "skip, only, and todo"
        [ test "empty list" <| 
            \_ -> List.permutation [] |> Expect.equal []
        , test "one element in list" <|
            \_ -> List.permutation [1] |> Expect.equal [[1]]    
        , test "two elements in list" <|
            \_ -> List.permutation [1,2] |> Expect.equal [[1,2], [2,1]]    
        , test "three elements in list" <| 
            \_ -> List.permutation [1,2,3] |> Expect.equal [[1,2,3], [1,3,2], [2,1,3], [2,3,1], [3,1,2], [3,2,1]]    
        ]
