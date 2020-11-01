module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Survey exposing (..)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    Survey


init : Model
init =
    { previous = []
    , current = question1
    , next = [ question2, question3 ]
    }



-- UPDATE


type Msg
    = NextQuestion
    | PreviousQuestion


update : Msg -> Model -> Model
update msg model =
    case msg of
        NextQuestion ->
            nextQuestion model

        PreviousQuestion ->
            previousQuestion model



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text model.current.text ]
        , button
            [ onClick PreviousQuestion
            , class "bg-yellow-500 text-black font-bold py-2 px-4"
            ]
            [ text "◀ Previous" ]
        , button
            [ onClick NextQuestion
            , class "bg-yellow-500 text-black font-bold py-2 px-4"
            ]
            [ text "Next ▶" ]
        ]
