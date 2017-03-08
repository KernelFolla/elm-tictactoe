-- @author Marino Di Clemente <kernelfolla@gmail.com>

-- two reason to use elm: great performance and no runtime exceptions
-- to reason to not use it: learn functional programming isn't easy, javascript
-- is still enough cool!

-- importing modules, for the delight of Python/ES6 developers
import Html exposing (beginnerProgram, div, button, text)
import Html.Events exposing (onClick)
import Array exposing (repeat, get, set, Array)
-- here i define an union type(someone calls it enum, someone tagged union and
-- matematicians calls it algebraic data type)
-- @see https://dennisreimann.de/articles/elm-data-structures-union-type.html
-- for simplicity i consider draw as a player, empty cells are filled by the DRAW
-- and matches in draw are recorder as won by DRAW
type Player = X | O | DRAW

-- here the alias type to define the state of the application
-- the core idea is that your code is built around a Model of your application
-- state, a way to update your model, and a way to view your model.
-- @see https://guide.elm-lang.org/architecture/
type alias Model =
  { current: Player
  , cells: Array Player
  , games: List Player
  }

-- an easy function that gives me the opponent player,
-- interesting to see that if i don't add a case for DRAW it says:
-- "This `case` does not have branches for all possibilities."
-- no way out:) and hey, it's our first function in elm!
-- @see https://www.elm-tutorial.org/en/01-foundations/02-functions.html
opponent : Player -> Player
opponent player =
  case player of
      X -> O
      O -> X
      DRAW -> DRAW

-- this is another simple function to transform a player type in a String
-- the extra effor is the use of Maybe, because lists and arrays can return a Player
-- but maybe Nothing
-- @see http://rundis.github.io/blog/2016/elm_maybe.html
-- @see http://package.elm-lang.org/packages/elm-lang/core/latest/Maybe
playerToString : Maybe Player -> String
playerToString player =
  case player of
      Just X -> "X"
      Just O -> "O"
      Just DRAW -> "_"
      Nothing -> "-"

-- this is the rendering of a single cell, passing the position on the click event,
-- using Array.get to retrieve the value and using |> to have a nice syntax
-- @see https://guide.elm-lang.org/architecture/user_input/buttons.html
-- @see http://package.elm-lang.org/packages/elm-lang/core/latest/Array#get
-- @see http://package.elm-lang.org/packages/elm-lang/core/2.1.0/Basics#|>
cell : Model -> Int -> Html.Html Int
cell model pos = button [ onClick pos  ] [ get (pos - 1) model.cells |> playerToString |> text ]

-- a function to render the grid of buttons in a react-like way,
-- in the last line I use a toString function, it's the var_dump of php,
-- the print or rails or pprint of pythong, really useful to understand how
-- things are going
-- @todo use function composition to be a little more DRY on "cell model x"
-- @todo use elmx (https://github.com/pzavolinsky/elmx)
--
view model =
  div []
    [
    div [] [ cell model 1, cell model 2, cell model 3 ]
    , div [] [ cell model 4 , cell model 5 , cell model 6]
    , div [] [ cell model 7, cell model 8, cell model 9 ]
    , div [] [ text (toString model) ]
    ]

-- when i click a button we can update the state so we receive the message (pos)
-- and the previous state (model),
-- here is the hard part of the logic, i need to return an updated Model with
-- the opponent player but also find the winner or understand if the game is
-- finished in draw, here if are welcome but also i'm using let to do something
-- called "destructuring assignment", in other words you split all the nesting
-- flow of functions in something that looks procedural
-- @todo understand why I can't use the function signature here
-- @see http://elm-lang.org/docs/syntax#let-expressions
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

-- here I check if there are cells filled by DRAW, if true it means that players
-- can't go forward and this means it's a draw game
checkDraw : Player -> Array Player -> Bool
checkDraw winner cells =
  (winner == DRAW) && (
    cells
    |> Array.toList
    |> List.filter (\n -> n == DRAW)
    |> List.isEmpty
  )

-- to understand if someone won i use a static list of possible cases and I map
-- them using another function called checkCase, of course checkCase needs to
-- know also how cells are filled so i'm using an anonym function
-- @see https://www.elm-tutorial.org/en/01-foundations/02-functions.html
-- @todo if checkCase returns the winner, i'd like to know also the valid case
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

-- it's almost the end! I just need a function that understands if the case
-- (that's a list of 3 numbers) matches to 3 cells filled by the same user
-- and here i'm using all the force of lists, mapping the list with its
-- corresponding cell values, and after i do that i do a reduce (it's called
-- fold in elm) using the first value of list(head) and the remaining values
-- (tail), Maybe.withDefault comes in help because without it elm thinks the
-- returned value could be Nothing and this means we have a Maybe Player instead
-- of a Player, we know there is always a player so we force it using
--  Maybe.withDefault
-- @see http://package.elm-lang.org/packages/elm-lang/core/latest/List
-- @see (again) http://rundis.github.io/blog/2016/elm_maybe.html
checkCase : List Int -> Array Player -> Player
checkCase l cells =
  let
    ll = List.map (\n -> get (n-1) cells |> Maybe.withDefault DRAW) l
  in
    List.foldl
     (\a b -> if a == b then a else DRAW)
     (List.head ll |> Maybe.withDefault DRAW)
     (List.tail ll |> Maybe.withDefault [DRAW])

-- here the core of the app, the main() of C, the createStore of redux
-- (very similar), the AppKernel of Symfony(definitely not similar sorry:) )
-- it defines the initial state, the rendering function and a function able to
-- transform the couple of state+message into another state
-- that's it!
main = beginnerProgram {
    model =  {
      current = X
      , cells = repeat 9 DRAW
      , games = []
    }
    , view = view
    , update = update
  }
