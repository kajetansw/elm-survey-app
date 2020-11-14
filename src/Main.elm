module Main exposing (..)

import Browser
import Html exposing (Html, button, div, img, input, label, option, p, select, span, text, textarea)
import Html.Attributes exposing (checked, class, disabled, rows, selected, src, type_, value)
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
    , next = [ question2, question3, question4, question5 ]
    }



-- UPDATE


type Msg
    = NextQuestion
    | PreviousQuestion
    | SelectAnswerChanged String
    | TextAnswerChanged String
    | CheckboxAnswerChanged String Bool
    | RateAnswerChanged Int


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
                TextAnswer _ ->
                    let
                        currentQuestion =
                            model.current

                        updatedAnswer =
                            TextAnswer value
                    in
                    { model | current = { currentQuestion | answer = updatedAnswer } }

                _ ->
                    model

        CheckboxAnswerChanged txt value ->
            case model.current.answer of
                CheckboxAnswer ls ->
                    let
                        currentQuestion =
                            model.current

                        updatedAnswer =
                            CheckboxAnswer (pickCheckboxAnswer txt value ls)
                    in
                    { model | current = { currentQuestion | answer = updatedAnswer } }

                _ ->
                    model

        RateAnswerChanged rate ->
            case model.current.answer of
                RateAnswer range _ ->
                    let
                        currentQuestion =
                            model.current

                        updatedRate =
                            if rate >= 1 && rate <= range then
                                Just rate

                            else
                                Nothing

                        updatedAnswer =
                            RateAnswer range updatedRate
                    in
                    { model | current = { currentQuestion | answer = updatedAnswer } }

                _ ->
                    model



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "w-full h-full flex flex-col items-center" ]
        [ div [ class "sm:w-10/12 lg:w-8/12 h-full p-12 mt-12 mb-12 bg-white shadow-md rounded flex flex-col justify-between items-center sm:rounded-xl lg:rounded-md" ]
            [ div [ class "flex flex-col items-center" ]
                [ span [ class "text-4xl text-gray-500 mb-10" ] [ currentQuestionNoText model ]
                , p [ class "text-6xl inline-block text-center" ] [ text model.current.text ]
                ]
            , div [ class "text-5xl flex flex-col items-center w-full" ]
                [ viewAnswer model ]
            , div [ class "flex flex-row w-full text-4xl" ]
                [ button
                    [ onClick PreviousQuestion
                    , class "bg-yellow-500 text-black font-bold py-8 px-4 w-3/6 mr-4 rounded"
                    ]
                    [ text "◀ Previous" ]
                , button
                    [ onClick NextQuestion
                    , class "bg-yellow-500 text-black font-bold py-2 px-4 w-3/6 ml-4 rounded"
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
            let
                isChecked : String -> Bool
                isChecked txt =
                    let
                        matchingValues =
                            List.filter (\l -> l.text == txt) ls
                    in
                    case matchingValues of
                        [] ->
                            False

                        _ ->
                            List.all (\l -> l.value == True) matchingValues

                labeledValuesToHtml : List (Html Msg)
                labeledValuesToHtml =
                    List.map
                        (\l ->
                            label [ class "flex items-center mb-12" ]
                                [ input
                                    [ type_ "checkbox"
                                    , class "form-checkbox border-gray-700 mr-6 rounded-md"
                                    , checked (isChecked l.text)
                                    , onClick (CheckboxAnswerChanged l.text (not l.value))
                                    ]
                                    []
                                , span [ class "ml-2" ] [ text l.text ]
                                ]
                        )
                        ls
            in
            div [ class "flex flex-col mt-6" ]
                labeledValuesToHtml

        SelectAnswer answer ->
            let
                isSelected : String -> Bool
                isSelected option =
                    case answer.picked of
                        Nothing ->
                            option == answer.default

                        Just p ->
                            option == p

                optionToHtml : String -> Html Msg
                optionToHtml txt =
                    option [ value txt, selected (isSelected txt), disabled (txt == answer.default) ] [ text txt ]
            in
            select
                [ class "form-select w-10/12 mt-1 text-5xl"
                , onChange (\val -> SelectAnswerChanged val)
                ]
                (List.map optionToHtml (answer.default :: answer.options))

        TextAnswer txt ->
            textarea
                [ class "form-textarea w-full text-5xl"
                , rows 8
                , onChange (\val -> TextAnswerChanged val)
                ]
                [ text txt ]

        RateAnswer range rate ->
            let
                rates : List Int
                rates =
                    List.range 1 range

                rateToHtml : List (Html Msg)
                rateToHtml =
                    case rate of
                        Just rateInt ->
                            List.map
                                (\r ->
                                    if r <= rateInt && List.member rateInt rates then
                                        img
                                            [ src "./assets/star-on.png"
                                            , class "cursor-pointer"
                                            , onClick (RateAnswerChanged r)
                                            ]
                                            []

                                    else
                                        img
                                            [ src "./assets/star-off.png"
                                            , class "cursor-pointer"
                                            , onClick (RateAnswerChanged r)
                                            ]
                                            []
                                )
                                rates

                        _ ->
                            List.map (\r -> img [ src "./assets/star-off.png", class "cursor-pointer", onClick (RateAnswerChanged r) ] []) rates
            in
            div [ class "flex flex-row" ] rateToHtml
