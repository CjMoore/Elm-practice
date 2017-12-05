import Html exposing (text)

list1 = [1,2,3,4,5]
list2 = []

isEmpty : List a -> Bool
isEmpty list = 
   List.foldr (\item bool -> False ) True list


-- 1) Recursive
-- 2) Using Foldl
length : List a -> Int
length list = 
  case list of
    [] ->
      0
    x::tail ->
      1 + (length tail)
      
      
reverse1 : List a -> List a
reverse1 list = 
  List.foldl (::) [] list
      
reverse2 : List a -> List a
reverse2 list =
  case list of 
    [] -> 
      []
    x::tail ->
      (reverse2 tail) ++ [x]
      
member1 : List a -> a -> Bool
member1 list member = 
  List.foldr (\item bool -> if item == member then True else bool) False list  

member2 : List a -> a -> Bool
member2 list isIncluded = 
  case list of
    [] -> 
      False
    x::tail ->
      if x == isIncluded then
        True
      else
        member2 tail isIncluded
        
        
head : List a -> Maybe a 
head list = 
  case list of
    [] ->
      Nothing
    x::tail ->
     Just x
     
tail : List a -> Maybe (List a)
tail list = 
  case list of 
    [] ->
      Nothing
    x::tail ->
      Just tail
                
                
filter1 : (a -> Bool) -> List a -> List a
filter1 f list = 
  List.foldr (\item newList -> if (f item) then newList ++ [item] else newList) [] list

filter2 : (a -> Bool) -> List a -> List a
filter2 f list = 
  case list of
    [] ->
      []
    x::tail ->
      if (f x) then
        (filter2 f tail) ++ [x]
      else
        filter2 f tail 
        

isEven : Int -> Bool
isEven num = 
  (num % 2) == 0
  
  
take1 : Int -> List a -> List a
take1 num list = 
  List.foldl (\item newList 
                -> if ((List.length newList) < num) then
                       newList ++ [item]
                    else
                      newList
             ) [] list       
             
   
take2 : Int -> List a -> List a
take2 num list =
  case list of 
    [] ->
      []
    x::tail ->
      if num == 0 then
        []
      else
        [x] ++ (take2 (num - 1) tail)
        
        
take3 : Int -> List a -> List a
take3 num list = 
  List.foldl (\item listTuple
                -> if (Tuple.second listTuple) == 0 then
                     (Tuple.first listTuple, 0)
                   else
                     (((Tuple.first listTuple) ++ [item]), ((Tuple.second listTuple) - 1))
             ) ([], num) list
  |> Tuple.first            
          
drop1 :  Int -> List a -> List a       
drop1 num list = 
  List.foldl (\item listTuple
                -> if (Tuple.second listTuple) < num then
                     (Tuple.first listTuple, (1+ Tuple.second listTuple))
                   else
                     (((Tuple.first listTuple) ++ [item]), num)
             ) ([], 0) list
  |> Tuple.first   
  
  
drop2 : Int -> List a -> List a
drop2 num list =
  case list of 
    [] ->
      []
    x::tail ->
      if num == 0 then
         list
      else
        drop2 (num - 1) tail  


concat : List (List a) -> List a
concat listOfLists = 
  List.foldl (\item list -> list ++ item) [] listOfLists
  
  
concat2 : List (List a) -> List a
concat2 listOfLists = 
  case listOfLists of 
    [] ->
      []
    x::tail ->
      x ++ (concat2 tail)
  
(+++) : List a -> List a -> List a
(+++) listOne listTwo = 
  case listOne of 
    [] ->
      listTwo
    x::tail ->
      (::) x ((+++) tail listTwo)
      

intersperse1 : a -> List a -> List a
intersperse1 newThing list = 
  List.foldl (\item newList -> newList ++ [item] ++ [newThing]) [] list 
  
  
intersperse2 : a -> List a -> List a
intersperse2 addThis list =
  case list of 
    [] ->
      []
    x::tail ->
      [x] ++ [addThis] ++ (intersperse2 addThis tail)

--length [1, 2]
--1 + length [1]
--1 + 1 + length []
--1 + 1 + 0

partition1 : (a -> Bool) -> List a -> (List a, List a)
partition1 f list = 
 List.foldl (\item newTuple -> if (f item) then 
                                 (((Tuple.first newTuple) ++ [item]), (Tuple.second newTuple))
                               else
                                 ((Tuple.first newTuple), ((Tuple.second newTuple) ++ [item]))
            ) ([],[]) list
            
            
partition2 : (a -> Bool) -> List a -> (List a, List a)
partition2 f list = 
  case list of
    [] ->
      ([], [])
    x::tail ->
      let
        (trueInTail, falseInTail) = partition2 f tail
      in
        if (f x) then
          (([x] ++ trueInTail), falseInTail)
        else
          (trueInTail, ([x] ++ falseInTail))
          
          
unzip1 : List (a, b) -> (List a, List b)
unzip1 list = 
  List.foldl (\item newTuple -> ( (Tuple.first newTuple ++ [Tuple.first item])
                                , (Tuple.second newTuple ++ [Tuple.second item])
                                ) ) ([], []) list
                                
                                
unzip2 : List (a, b) -> (List a, List b)
unzip2 list = 
  case list of
    [] ->
      ([], [])
    x::tail ->
      let
        (firstPart, secondPart) = unzip2 tail
      in
        (([(Tuple.first x)] ++ firstPart ), ([Tuple.second x] ++ secondPart))
  
map1 : (a -> b) -> List a -> List b
map1 f list = 
  List.foldr (\item newList -> [(f item)] ++ newList) [] list

mapFirst2 : (a -> b) -> List a -> List b
mapFirst2 f list = 
  case list of
    [] ->
      []
    x::tail ->
      [(f x)] ++ mapFirst2 f tail

mapTwo1 : (a -> b -> result) -> List a -> List b -> List result
mapTwo1 f list1 list2 =
  List.foldl (\item newTuple -> let
                                  item2 = Array.get (Tuple.second newTuple) (Array.fromList list2)
                                in
                                  case item2 of
                                    Just num ->
                                      ((Tuple.first newTuple) ++ [(f item num)]
                                      , ((Tuple.second newTuple + 1)))
                                    Nothing ->
                                      newTuple
            ) ([], 0) list1

             |> Tuple.first


mapTwo2 : (a -> b -> result) -> List a -> List b -> List result
mapTwo2 f list1 list2 =
  case (list1, list2) of
    ([], []) ->
      []
    (x::tail1, y::tail2) ->
      [f x y] ++ (mapTwo2 f tail1 tail2)
    _ ->
      []

filterMap1 : (a -> Maybe b) -> List a -> List b
filterMap1 f list1 =
  List.foldl (\item newList->
    let
      num = f item
    in
      case num of
        Just n ->
          newList ++ [n]
        Nothing ->
          newList
    ) [] list1


filterMap2 : (a -> Maybe b) -> List a -> List b
filterMap2 f list1 =
  case list1 of
    [] ->
      []
    x::tail ->
      let
        num = f x
      in
        case num of
          Just n ->
            [n] ++ filterMap2 f tail
          Nothing ->
            filterMap2 f tail

 
sum1 : List number -> number
sum1 list =
  List.foldl (\item num -> num + item) 0 list
  
  
sum2 : List number -> number
sum2 list = 
  case list of 
    [] ->
      0
    x::tail ->
      x + (sum2 tail)

product1 : List number -> number
product1 list = 
  List.foldl (\item num -> item * num) 1 list
  
  
product2 : List number -> number
product2 list = 
  case list of 
    [] ->
      1
    x::tail ->
      x * (product2 tail)


output = 
  sum2 list1
  
  
  
  
add : Int -> Int -> Int
add num1 num2 = 
  num1 + num2

timesTwo : Int -> Int
timesTwo num = 
  num * 2
  
-- partition, unzip, map, map2, filterMap, concatMap, indexedMap, sum, product, maximum, minimum, all, any  
  

  
  
main =
  text (toString output)
