module Sudoku exposing (main)

--imports
import Random as R
import Browser
import Html exposing (Html)
import Html.Events exposing (onClick)
import Html.Attributes as Attr exposing (style)
import Array as A
import Collage exposing (Collage)
import Collage.Text as Text exposing (Text, fromString)
import Collage.Render as Render exposing (svgBox)
import Collage.Layout as Lay exposing (vertical)
import Color exposing (Color)
import Random exposing (..)
import Browser.Events exposing (onMouseDown)
import Json.Decode as Decode exposing (..)
import Random.List as RL

--DATA TYPES
type alias Model =
  {
    -- board is the state of the puzzle that the user sees
    board : A.Array Int,

    -- difficulty corresponds to:  0-fullboard 1-easy, 2-med, 3-hard
    difficulty : Int,

    --list of indices that are empty
    emp : List Int,

    --random list of numbers 1 to 9 to use for backtracking, static
    randList : List Int,

    -- random list of numbers 0 60 to use for removing indices
    randInd : List Int

  }

type alias Board = A.Array Int

type alias Flags = ()

type Msg = Initialize | MakeList (List Int) | GenerateBoard | MakeIndList (List Int) | SetEasy | SetMedium | SetHard | Reset

--type Msg = GenerateRandomValue | UpdateCell Int | Initialize | MakeList (List Int) | GenerateBoard


--------------------------------------MAIN--------------------------------------
main : Program Flags Model Msg
main =
  Browser.element
    { init = init,
      view = view,
      update = update,
      subscriptions = subscriptions
    }


--------------------------------INITIALIZING------------------------------------

init : Flags -> (Model, Cmd Msg)
init () =
  (initialize, Cmd.none)

initModel : Model
initModel =
  {
    board = A.repeat 81 0,
    difficulty = 0,
    emp = List.range 0 80,
    randList = List.range 1 9,
    randInd = List.range 0 60
  }

--this function updates the randList and shuffles it
initialize : Model
initialize =
  case (update Initialize initModel) of
    (newmod, _) -> newmod

---------------------------------UPDATING---------------------------------------

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [
      --Browser.Events.onMouseDown (Decode.succeed Initialize)
    ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Initialize ->
      (
        let _ = Debug.log "init" ()
        in
        initModel, Cmd.batch [Random.generate MakeList rList, Random.generate MakeIndList rIList]
      )
    MakeList newList ->
      (
        update GenerateBoard (initializeList model newList)
      )
    MakeIndList newList ->
      (
        (initializeRList model newList), Cmd.none
      )
    GenerateBoard ->
      (
        (makeFinalSudoku model), Cmd.none
      )
    SetEasy ->
      (
        let _ = Debug.log "SetEasy" ()
        in
        update GenerateBoard (setEasy model)
      )
    SetMedium ->
      (
        let _ = Debug.log "SetMedium" ()
        in
        update GenerateBoard (setMedium model)
      )
    SetHard ->
      (
        let _ = Debug.log "SetHard" ()
        in
        update GenerateBoard (setHard model)
      )
    Reset ->
      (
        let _ = Debug.log "Reset" ()
        in
        update Initialize model
      )

initializeList : Model -> List Int -> Model
initializeList m l =
  {
    board = A.repeat 81 0,
    difficulty = m.difficulty,
    emp = m.emp,
    randList = l,
    randInd = m.randInd
  }

initializeRList : Model -> List Int -> Model
initializeRList m l =
  {
    board = A.repeat 81 0,
    difficulty = m.difficulty,
    emp = m.emp,
    randList = m.randList,
    randInd = l

  }

setEasy : Model -> Model
setEasy m =
  {
    board = m.board,
    difficulty = 1,
    emp = m.emp,
    randList = m.randList,
    randInd = m.randInd 
  }

setMedium : Model -> Model
setMedium m =
  {
    board = m.board,
    difficulty = 2,
    emp = m.emp,
    randList = m.randList,
    randInd = m.randInd 
  }

setHard : Model -> Model
setHard m =
  {
    board = m.board,
    difficulty = 3,
    emp = m.emp,
    randList = m.randList,
    randInd = m.randInd 
  }

reset : Model -> Model
reset m =
  {
    board = A.repeat 81 0,
    difficulty = 0,
    emp = List.range 0 80,
    randList = List.range 1 9,
    randInd = List.range 0 80 
  }

view : Model -> Html Msg
view model =
  let
    styles =
      [ ("position", "fixed"),
        ("top", "50%"),
        ("left", "50%"),
        ("transform", "translate(-50%, -75%)")
      ]
    display =
      Render.svgBox (450, 450) (modelToCollage model)
    resetb = 
      Html.button [onClick Reset] [Html.text "New Board"]
    easy =
      Html.button [onClick SetEasy] [Html.text "Easy"]
    medium =
      Html.button [onClick SetMedium] [Html.text "Medium"]
    hard =
      Html.button [onClick SetHard] [Html.text "Hard"]
  in
  Html.div (List.map (\(k, v) -> Attr.style k v) styles)
  --Html.div []
    [
      resetb, easy, medium, hard, display
    ]

----------------- FUNCTIONS FOR GENERATING RANDOM SUDOKU BOARD -----------------
--Random list generator
rList : Random.Generator (List Int)
rList =
  RL.shuffle [1,2,3,4,5,6,7,8,9]

rIList : Random.Generator (List Int)
rIList =
  RL.shuffle (List.range 0 80)

--Helper Function to unwrap Array.get
getWrap : Int -> (A.Array Int) -> Int
getWrap index arr =
  case (A.get index arr) of
    Nothing -> Debug.todo "getWrap: this shouldn't happen!"
    Just x -> x

--checks that there is no same number in that row for that position
checkRow : A.Array Int -> Int -> Int -> Bool
checkRow board pos val =
  let
    rowBegin = 9 * (pos // 9)
    rowEnd = rowBegin + 8
    row = A.slice rowBegin rowEnd board
  in
  A.isEmpty(A.filter (\x -> x==val) row)

--same logic as checkRow, except for columns
--creates a list of indexes to check, then maps it to values
checkCol : A.Array Int -> Int -> Int -> Bool
checkCol board pos val =
  let
    rowNum = pos//9 --row number position is in
    rowStart = rowNum * 9 --index of beginning of row
    colBegin = pos - rowStart
    indexArr = A.initialize 9 (\n -> colBegin + (n*9))
    col = A.map (\n -> getWrap n board) indexArr
  in
  A.isEmpty(A.filter (\x -> x==val) col)

--same logic as checkCol, for squares
checkSquare : A.Array Int -> Int -> Int -> Bool
checkSquare board pos val =
  let
    mod27 = modBy 27 pos --helper variable for row numbering
    row =
      if (mod27 < 9)
        then 1
      else if (mod27 < 18)
        then 2
      else 3
    col =
      case (modBy 3 pos) of
        0 -> 1
        1 -> 2
        2 -> 3
        _ -> Debug.todo "checkSquare: this shouldn't happen!"
    indexList =
      case (row, col) of
        (1, 1) -> [pos+10,pos+11,pos+19,pos+20]
        (1, 2) -> [pos+8,pos+10,pos+17,pos+19]
        (1, 3) -> [pos+7,pos+8,pos+16,pos+17]
        (2, 1) -> [pos-7,pos-8,pos+10,pos+11]
        (2, 2) -> [pos-10,pos-8,pos+8,pos+10]
        (2, 3) -> [pos-11,pos-10,pos+7,pos+8]
        (3, 1) -> [pos-17,pos-16,pos-8,pos-7]
        (3, 2) -> [pos-19,pos-17,pos-10,pos-8]
        (3, 3) -> [pos-20,pos-19,pos-11,pos-10]
        (_, _) -> Debug.todo "checkSquare: this shouldn't happen!"
    sq = List.map (\n -> getWrap n board) indexList
  in
  List.isEmpty (List.filter (\x -> x==val) sq)

--checks all three booleans for all constraints
checkAll : A.Array Int -> Int -> Int -> Bool
checkAll board pos val =
  (checkRow board pos val) &&
    (checkCol board pos val) &&
      (checkSquare board pos val)

--Recursive Backtracking sudoku solver
backSolve : (A.Array Int) -> (List Int) -> (List Int) -> (List Int) -> (A.Array Int)
backSolve board emps rand fullRand =
  case emps of
    [] -> board
    i::rest -> --this is the list of indices, i is the one we'll try
      case rand of --this is the list of numbers we haven't tried yet
        [] -> board --this makes solvable return false
        try::tryRest -> --try is the new number we can try
          if checkAll board i try
          then
            let
              newBoard = A.set i try board
              trySolve = backSolve newBoard rest fullRand fullRand
            in
            if solvable(trySolve)
              then
                trySolve
            else
              backSolve board emps tryRest fullRand
          else
            backSolve board emps tryRest fullRand

--helper function that helps backtracks if board is not solvable
solvable : A.Array Int -> Bool
solvable board =
  A.isEmpty (A.filter (\x -> x==0) board) --if there is a 0, it returns false

--function to generate a full board
newFullBoard : Model -> Model
newFullBoard m =
  let
    newM = {m | board = (backSolve m.board m.emp m.randList m.randList)}
  in
  {
    board = newM.board,
    difficulty = newM.difficulty,
    emp = [],
    randList = newM.randList,
    randInd = newM.randInd
  }

backSolveCount : A.Array Int -> List Int -> List Int -> List Int -> Int
backSolveCount board emps possibleRest fullPossible =
  case emps of
    [] -> 1
    i::rest ->
      case possibleRest of
        [] -> 0
        try::tryRest ->
          if checkAll board i try then
            let
              newBoard = A.set i try board
              trySolve = backSolve newBoard rest fullPossible fullPossible
            in
              if solvable(trySolve) then
                1 + backSolveCount board emps tryRest fullPossible
              else
                backSolveCount board emps tryRest fullPossible
          else
            backSolveCount board emps tryRest fullPossible

--function to take away squares from full board
--NEED TO DO: unique sequence of cells to take away
takeAway : Model -> Int -> Model
takeAway m attempt =
  case attempt of
    0 -> m
    a ->
      -- in each attempt, set the value of a random index to 0
      -- call uniqueSol on the new board with one missing value
      let
        index = Maybe.withDefault -1 (List.head (List.drop (attempt - 1) (List.take attempt m.randInd)))
        newBoard = A.set index 0 m.board
        newEmp = [index]++m.emp
        possibleSolutions = backSolveCount newBoard newEmp (List.range 1 9) (List.range 1 9)
        _ = Debug.log "difficulty" m.difficulty
      in
      if possibleSolutions == 1 then
        takeAway {m | board = newBoard, emp = newEmp} (attempt - 1)
      else
        takeAway m (attempt - 1)


--function to generate board based on difficulty
makeFinalSudoku : Model -> Model
makeFinalSudoku m =
  case m.difficulty of
    0 -> newFullBoard m
    1 -> takeAway (newFullBoard m) 20
    2 -> takeAway (newFullBoard m) 30
    _ -> takeAway (newFullBoard m) 40


-------------- FUNCTIONS FOR RENDERING IMAGE OF SUDOKU BOARD -------------------
-- HELPER to stringifyBoard: takes in a 1D array of length 81, adds the newline
-- character '\n' to every 9
addNewlines : Int -> Int -> List String -> List String -> List String
addNewlines take drop strs out =
  if take <= 81 then
    if (modBy 9 take) == 0 then
      addNewlines (take + 1) (drop + 1) strs (out ++ (List.drop drop (List.take take strs)) ++ ["\n"])
    else if (modBy 3 take) == 0 then
      addNewlines (take + 1) (drop + 1) strs (out ++ (List.drop drop (List.take take strs)) ++ ["   |   "])
    else
      addNewlines (take + 1) (drop + 1) strs (out ++ (List.drop drop (List.take take strs)) ++ ["   "])
  else
    out

-- Takes a board and outputs a list of strings, each string is a row
stringifyBoard : Model -> List String
stringifyBoard m =
  String.lines(List.foldr (++) "" (addNewlines 1 0 (A.toList (A.map (String.fromInt) m.board)) []))

-- HELPER to modelToCollage: takes a string (row on board) and outputs a collage
makeText : String -> Collage msg
makeText str =
  Text.fromString (str)
  |> Text.size Text.huge
  |> Text.color Color.black
  |> Collage.rendered

-- HELPER to modelToCollage: adds horizontal line in threes
addLines : Int -> List (Collage msg) -> List (Collage msg) -> List (Collage msg)
addLines i curr cs =
  case cs of
    [] -> curr
    line::rest ->
      let
        vmark = Collage.traced (Collage.solid Collage.thin (Collage.uniform Color.black)) (Collage.line 200)
      in
      if (i==2) || (i==5) then
        addLines (i+1) (curr++[line,vmark]) rest
      else
        addLines (i+1) (curr++[line]) rest

-- takes the current model and converts it to a collage
modelToCollage : Model -> Collage msg
modelToCollage m =
  let
    vspace = Lay.spacer 0 2
    vmark = Collage.traced (Collage.solid Collage.thin (Collage.uniform Color.black)) (Collage.line 200)
    lines = addLines 0 [] (List.map makeText (stringifyBoard m))
    complete = List.intersperse vspace lines
  in
  Lay.vertical complete
