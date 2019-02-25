module BlackJack

type Card =
  private
  | Ace
  | Number of int
  | Jack
  | Queen
  | King
  
let cardForTestData = Number
let aceForTest = Card.Ace
let jackForTest = Card.Jack
let queenForTest = Card.Queen
let kingForTest = Card.King

let suit =
  [ Ace ]
  @ [ for i in 2 .. 10 -> Number i ]
  @ [ Jack; Queen; King ]
  
let heartSuit = suit
let diaSuit = suit
let cloverSuit = suit
let spadeSuit = suit

let deck =
  heartSuit
  @ diaSuit
  @ cloverSuit
  @ spadeSuit

let toPoint = function
  | Ace -> [ 1; 11 ]
  | Number n -> [ n ]
  | _ -> [ 10 ]
  
let plusEach list1 list2 =
  List.collect (fun element1 ->
    List.map (fun element2 ->
      element1 + element2
    ) list2
  ) list1
      
let sumHand cards =
  let possiblePoints = cards |> List.map toPoint
  let scoreCandidates = possiblePoints |> List.fold plusEach [0]
  scoreCandidates
  |> List.where ((>=) 21)
  |> function
  | [] -> scoreCandidates |> List.min
  | noBusts -> noBusts |> List.max
