module ListPermutation exposing(permutation)

permutation : List Int -> List (List Int)
permutation inputList = 
  case inputList of
    [] ->
      []
    [x] ->
      [[x]]
    inputList -> 
      List.indexedMap (\ index element ->
         let
           beforeElement = List.take index inputList
           afterElement = List.drop (index + 1) inputList
         in 
          permutation (beforeElement ++ afterElement)  
          |> List.map (\ list2 -> [element] ++ list2)
        ) inputList 
        |> List.concat
