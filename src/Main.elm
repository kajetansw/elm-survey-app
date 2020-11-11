module Main exposing (..)

import Browser
import Html exposing (Html, button, div, option, select, span, text, textarea)
import Html.Attributes exposing (class, selected, value)
import Html.Events exposing (onClick)
import Html.Events.Extra exposing (onChange)
import Survey exposing (..)
import Svg as SVG
import Svg.Attributes as SVGA



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
            div [] []

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
                    [ class "block appearance-none w-full bg-white border border-gray-400 hover:border-gray-500 px-4 py-2 pr-8 rounded shadow leading-tight focus:outline-none focus:shadow-outline"
                    , onChange (\val -> SelectAnswerChanged val)
                    ]
                    (List.map optionToHtml answer.options)
                , viewDropdownArrow
                ]

        TextAnswer text ->
            textarea [ class "shadow appearance-none border rounded w-full py-2 px-3 text-gray-700 leading-tight focus:outline-none focus:shadow-outline" ] []

        IntegerAnswer labeled ->
            div [] []


viewDropdownArrow : Html Msg
viewDropdownArrow =
    div [ class "pointer-events-none absolute inset-y-0 right-0 flex items-center px-2 text-gray-700" ]
        [ SVG.svg [ SVGA.class "fill-current h-4 w-4", SVGA.viewBox "0 0 20 20" ]
            [ SVG.path [ SVGA.d "M9.293 12.95l.707.707L15.657 8l-1.414-1.414L10 10.828 5.757 6.586 4.343 8z" ] [] ]
        ]
