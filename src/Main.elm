module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias LabeledValue v =
    { text : String
    , value : v
    }


type Answer
    = CheckboxAnswer (List (LabeledValue Bool))
    | TextAnswer (LabeledValue String)
    | IntegerAnswer (LabeledValue Int)
    | YesNoAnswer (LabeledValue Bool)


type Question
    = Question
        { text : String
        , answer : Answer
        }


type Survey
    = Survey
        { previous : List Question
        , current : Question
        , next : List Question
        }


type alias Model =
    Int


init : Model
init =
    0



-- UPDATE


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            model + 1

        Decrement ->
            model - 1



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button
            [ onClick Decrement
            , class "bg-blue-500 text-white font-bold py-2 px-4"
            ]
            [ text "-" ]
        , div [] [ text (String.fromInt model) ]
        , button
            [ onClick Increment
            , class "bg-blue-500 text-white font-bold py-2 px-4"
            ]
            [ text "+" ]
        ]
