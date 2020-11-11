module Main exposing (..)

import Browser
import Html exposing (Html, button, div, input, label, option, select, span, text, textarea)
import Html.Attributes exposing (class, selected, type_, value)
import Html.Events exposing (onClick)
import Html.Events.Extra exposing (onChange)
import Survey exposing (..)



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    Survey


init : Model
init =
    { previous = []
    , current = question1
    , next = [ question2, question3, question4, question5, question6 ]
    }



-- UPDATE


type Msg
    = NextQuestion
    | PreviousQuestion
    | SelectAnswerChanged String
    | TextAnswerChanged String


update : Msg -> Model -> Model
update msg model =
    case msg of
        NextQuestion ->
            nextQuestion model

        PreviousQuestion ->
            previousQuestion model

        SelectAnswerChanged value ->
            case model.current.answer of
                SelectAnswer sa ->
                    let
                        currentQuestion =
                            model.current

                        updatedAnswer =
                            SelectAnswer (pickSelectAnswer sa value)
                    in
                    { model | current = { currentQuestion | answer = updatedAnswer } }

                _ ->
                    model

        TextAnswerChanged value ->
            case model.current.answer of
                TextAnswer txt ->
                    let
                        currentQuestion =
                            model.current

                        updatedAnswer =
                            TextAnswer value
                    in
                    { model | current = { currentQuestion | answer = updatedAnswer } }

                _ ->
                    model



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "w-full max-w-md" ]
        [ div [ class "bg-white shadow-md rounded px-8 pt-6 pb-8 mb-4 flex flex-col items-center" ]
            [ span [ class "text-base text-gray-500" ] [ currentQuestionNoText model ]
            , span [ class "text-xl inline-block" ] [ text model.current.text ]
            , viewAnswer model
            , div [ class "flex flex-row w-full" ]
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


currentQuestionNoText : Survey -> Html Msg
currentQuestionNoText s =
    text ((s |> currentQuestionNumber |> String.fromInt) ++ "/" ++ (s |> surveyLength |> String.fromInt))


viewAnswer : Survey -> Html Msg
viewAnswer s =
    case s.current.answer of
        CheckboxAnswer ls ->
            div [ class "flex mt-6" ]
                [ label [ class "flex items-center" ]
                    [ input [ type_ "checkbox", class "form-checkbox" ] []
                    , span [ class "ml-2" ] [ text "I agree" ]
                    ]
                ]

        SelectAnswer answer ->
            let
                isSelected : String -> Bool
                isSelected option =
                    answer.picked == option

                optionToHtml : String -> Html Msg
                optionToHtml txt =
                    option [ value txt, selected (isSelected txt) ] [ text txt ]
            in
            div [ class "inline-block relative w-full" ]
                [ select
                    [ class "form-select block w-full mt-1"
                    , onChange (\val -> SelectAnswerChanged val)
                    ]
                    (List.map optionToHtml answer.options)
                ]

        TextAnswer txt ->
            textarea
                [ class "form-textarea w-full"
                , onChange (\val -> TextAnswerChanged val)
                ]
                [ text txt ]

        IntegerAnswer labeled ->
            div [] []
