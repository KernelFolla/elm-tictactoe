import Html exposing (beginnerProgram, div, button, text)
import Html.Events exposing (onClick)
import Array exposing (repeat, get, set, Array)

type Player = X | O | DRAW

type alias Model =
  { current: Player
  , cells: Array Player
  , games: List Player
  }

opponent : Player -> Player
opponent player =
  case player of
      X -> O
      O -> X
      DRAW -> DRAW

playerToString : Maybe Player -> String
playerToString player =
  case player of
      Just X -> "X"
      Just O -> "O"
      Just DRAW -> "_"
      Nothing -> "-"

cell model pos = button [ onClick pos  ] [ get (pos - 1) model.cells |> playerToString |> text ]

view model =
  div []
    [
    div [] [ cell model 1, cell model 2, cell model 3 ]
    , div [] [ cell model 4 , cell model 5 , cell model 6]
    , div [] [ cell model 7, cell model 8, cell model 9 ]
    , div [] [ text (toString model) ]
    ]

update pos model =
  if get (pos-1) model.cells == Just DRAW
  then
    let
      cells = set (pos-1) model.current model.cells
      winner = checkWinner cells
      isFinished = winner /= DRAW || (checkDraw winner cells)
    in
      {
      current = model.current |> opponent
      , cells = if isFinished then repeat 9 DRAW else cells
      , games =
        if isFinished
        then List.append model.games [winner]
        else model.games
      }
  else model

checkDraw : Player -> Array Player -> Bool
checkDraw winner cells =
  (winner == DRAW) && (
    cells
    |> Array.toList
    |> List.filter (\n -> n == DRAW)
    |> List.isEmpty
  )

checkCase : List Int -> Array Player -> Player
checkCase l cells =
  let
   ll = List.map (\n -> get (n-1) cells |> Maybe.withDefault DRAW) l
   lll =
    List.foldl
      (\a b -> if a == b then a else DRAW)
      (List.head ll |> Maybe.withDefault DRAW)
      (List.tail ll |> Maybe.withDefault [DRAW])
  in
    lll

checkWinner : Array Player -> Player
checkWinner cells =
  let
    cases = [
      [1,2,3] , [4,5,6] , [7,8,9]     --horizontal
      , [1,4,7] , [2,5,8] , [3,6,9]   --vertical
      , [1,5,9] , [3,5,7]             --diagonals
    ]
    matched = List.map (\n -> checkCase n cells) cases
  in
    List.filter (\n -> n /=  DRAW) matched
      |> List.head
      |> Maybe.withDefault DRAW

main = beginnerProgram {
    model =  {
      current = X
      , cells = repeat 9 DRAW
      , games = []
    }
    , view = view
    , update = update
  }
