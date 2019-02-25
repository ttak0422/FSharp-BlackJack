open Expecto
open BlackJack

[<Tests>]
let modelTests =
  testList "model test" [
    test "デッキの枚数" {      
      Expect.equal 52 deck.Length "52枚であるはず"
    }
    test "suit ace" {      
      Expect.equal aceForTest suit.[0] "suit.[0] = ace"
    }
    test "suit jqk" {
      Expect.equal jackForTest suit.[11 - 1] "suit.[10] = jack"
      Expect.equal queenForTest suit.[12 - 1] "suit.[11] = queen"
      Expect.equal kingForTest suit.[13 - 1] "suit.[12] = king"      
    }
    test "suit number" {
      for i in 2 .. 10 do
        let card = suit.[i - 1]
        let expected = cardForTestData i
        let msg = sprintf "suit.[%i] = [%i]" (i-1) i                         
        Expect.equal expected card msg
    }
    test "unknown number" {
      Expect.throws (fun _ -> suit.[14] |> ignore) "14番目のカードは存在しないよ"      
    }
  ]
  
[<Tests>]
let logicTests =
  
  let ace = suit.[0]
  let card2 = suit.[2 - 1]
  let card3 = suit.[3 - 1]
  let jack = suit.[11 - 1]
  let queen = suit.[12 - 1]
  let king = suit.[13 - 1]
  
  testList "logic test" [
    test "カードが与えられないとき" {
      let expected = 0
      let result = sumHand []
      Expect.equal expected result "0であるべき"
    }
    test "カードの合計" {
      let expected = 5
      let result = sumHand [card3; card2]
      Expect.equal expected result "3 + 2 = 5"
    }
    test "J,Q,Kが10として扱われる" {            
      Expect.equal (sumHand [suit.[2 - 1]; jack]) 12 "2 + ? = 12"
      Expect.equal (sumHand [suit.[3 - 1]; jack]) 13 "3 + ? = 13"
      Expect.equal (sumHand [jack; queen]) 20 "j + q = 20"
      Expect.equal (sumHand [queen; king]) 20 "q + k = 20"
      Expect.equal (sumHand [king; jack]) 20 "k + j = 20"
    }
    test "Aceが1と11をとる" {
      Expect.equal (sumHand [suit.[2 - 1]; ace]) 13 "ace as 11"
      Expect.equal (sumHand [jack; ace]) 21 "ace as 11"
      Expect.equal (sumHand [jack; ace; suit.[10 - 1]]) 21 "ace as 1"
      Expect.equal (sumHand [ace; ace; king; suit.[9 - 1]]) 21 "ace as 1"
    }
    test "手札の合計が21を超えたとき21以上の値が返される" {
      Expect.isGreaterThan (sumHand [jack; king; suit.[5 - 1]]) 21 "jack + king + 5 > 21" 
    }
  ]  

[<EntryPointAttribute>]
let main args =
  runTestsInAssembly defaultConfig args 
       
