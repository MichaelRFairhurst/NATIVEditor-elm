import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List exposing (..)
import String exposing(..)
import Keyboard exposing (KeyCode)

main =
  Html.program
    { init = (Model True 1 (toPieces "blah") "blah", Cmd.none)
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias Model =
  { editing : Bool
  , pos : Int
  , pieces : List Piece
  , text : String
  }

type Msg
  = ChangedText String
  | InsertChar Char
  | ChangedEditing Bool
  | MoveUp
  | MoveDown
  | MoveLeft
  | MoveRight
  | None

type Piece
  = Text String
  | SomeKeyword Keyword
  | Symbol Char
  | Newlines Int
  | Spaces Int
  | Tabs Int

type Keyword
  = For
  | If
  | While
  | Break


update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    None ->
      (model, Cmd.none)
    ChangedText str ->
      ({ model | pieces = toPieces str, text = str}, Cmd.none)
    InsertChar char ->
      update (ChangedText (insertChar char model.pos model.text) ) { model | pos = model.pos + 1 }
    ChangedEditing b ->
      ({ model | editing = b}, Cmd.none)
    MoveLeft ->
      ({ model | pos = max (model.pos - 1) 0}, Cmd.none)
    MoveRight ->
      ({ model | pos = (min (model.pos + 1) (String.length model.text))}, Cmd.none)
    MoveUp ->
      ({ model | pos = model.pos - 1}, Cmd.none)
    MoveDown ->
      ({ model | pos = model.pos + 1}, Cmd.none)

insertChar : Char -> Int -> String -> String
insertChar char pos text =
  let
    prefix = String.left pos text
    suffix = String.right ((String.length text) - pos) text
  in String.append prefix (char `String.cons` suffix)

toPieces : String -> List Piece
toPieces str =
  [Text str]

max a b = if a > b then a else b
min a b = if a < b then a else b

monospace = ("font-family", "monospace")
line = ("border-left", "1px solid black")
lineheight = ("height", "1em")
fontsize = ("font-size", "1em")

pieceStyle = style [fontsize, monospace]
cursorStyle model = style [monospace, fontsize, line, lineheight, ("marginLeft", ch model.pos), ("marginTop", "0.3em"), ("position", "absolute") ]
containerStyle = style [ ("border", "1px solid grey"), ("width", "250ch"), ("min-height", "80em"), ("padding", "5px"), ("margin", "5px") ]

ch x = (toString x) ++ "ch"

viewPiece : Piece -> Html Msg
viewPiece p =
  case p of
    Text t ->
       span [onClick (ChangedEditing True), pieceStyle ] [ text t ]
    _ ->
       span [] [ text "Non text type" ]

view : Model -> Html Msg
view model =
  div [ onClick (ChangedEditing True), containerStyle ]
    -- [ textarea [ onInput ChangedText ] [text model.text]
    (
      (cursor model)
      :: (List.map viewPiece model.pieces)
    )

cursor : Model -> Html Msg
cursor model = span [ cursorStyle model, hidden (not model.editing) ] []


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  if model.editing
  then Sub.batch
    [ Keyboard.ups (key model)
    --, Keyboard.downs (key model)
    ]
  else Sub.none

key : Model -> KeyCode -> Msg
key model keycode =
  case keycode of
    37 -> MoveLeft
    39 -> MoveRight
    40 -> ChangedEditing False
    38 -> ChangedText (model.text ++ "blah")
    65 -> InsertChar 'a'
    _ -> None
