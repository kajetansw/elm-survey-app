module Main exposing (..)

import Browser
import Html exposing (Html, button, div, form, h1, text)
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
    div [ class "w-full max-w-md" ]
        [ div
            [ class "bg-white shadow-md rounded px-8 pt-6 pb-8 mb-4 flex flex-col items-center" ]
            [ h1 [ class "text-xl inline-block" ] [ text model.current.text ]
            , div
                [class "flex flex-row w-full"]
                [ button
                    [ onClick PreviousQuestion
                    , class "bg-yellow-500 text-black font-bold py-2 px-4 w-3/6 mr-2 rounded"
                    ]
                    [ text "◀ Previous" ]
                , button
                    [ onClick NextQuestion
                    , class "bg-yellow-500 text-black font-bold py-2 px-4 w-3/6 ml-2 rounded"
                    ]
                    [ text "Next ▶" ]
                ]
            ]
        ]
