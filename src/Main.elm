module Main exposing (..)

import Browser
import Graphql.Http
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.SelectionSet exposing (SelectionSet)
import Html exposing (Html, a, button, div, img, input, label, main_, option, p, select, span, text, textarea)
import Html.Attributes exposing (checked, class, disabled, href, rows, selected, src, style, target, type_, value)
import Html.Events exposing (onClick)
import Html.Events.Extra exposing (onChange)
import RemoteData exposing (RemoteData(..))
import Survey.Core exposing (..)
import Survey.Questions exposing (..)
import Survey.Serialize exposing (..)
import Survey.Validators exposing (..)
import SurveyAPI.Mutation as Mutation
import SurveyAPI.Object.Survey as SurveyDocument
import SurveyAPI.Scalar as Scalar



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { survey : Survey
    , currentTime : String
    , submitResult : RemoteData (Graphql.Http.Error Scalar.Id) Scalar.Id
    }


init : String -> ( Model, Cmd Msg )
init currentTime =
    let
        model : Model
        model =
            { survey =
                { previous = []
                , current = question1
                , next = [ question2, question3, question4, question5 ]
                }
            , currentTime = currentTime
            , submitResult = NotAsked
            }
    in
    ( model, Cmd.none )



-- UPDATE


type Msg
    = NextQuestion
    | PreviousQuestion
    | SelectAnswerChanged String
    | TextAnswerChanged String
    | CheckboxAnswerChanged String Bool
    | RateAnswerChanged Int
    | SubmitSurvey Model
    | GotResponse (RemoteData (Graphql.Http.Error Scalar.Id) Scalar.Id)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NextQuestion ->
            ( { model | survey = nextQuestion model.survey }, Cmd.none )

        PreviousQuestion ->
            ( { model | survey = previousQuestion model.survey }, Cmd.none )

        SelectAnswerChanged value ->
            case model.survey.current.answer of
                SelectAnswer sa ->
                    let
                        newAnswer =
                            SelectAnswer (pickSelectAnswer sa value)
                    in
                    ( updateCurrentAnswer newAnswer model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        TextAnswerChanged value ->
            case model.survey.current.answer of
                TextAnswer _ ->
                    let
                        newAnswer =
                            TextAnswer value
                    in
                    ( updateCurrentAnswer newAnswer model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        CheckboxAnswerChanged txt value ->
            case model.survey.current.answer of
                CheckboxAnswer ls ->
                    let
                        newAnswer =
                            CheckboxAnswer (pickCheckboxAnswer txt value ls)
                    in
                    ( updateCurrentAnswer newAnswer model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        RateAnswerChanged rate ->
            case model.survey.current.answer of
                RateAnswer range _ ->
                    let
                        newRate =
                            if rate >= 1 && rate <= range then
                                Just rate

                            else
                                Nothing

                        newAnswer =
                            RateAnswer range newRate
                    in
                    ( updateCurrentAnswer newAnswer model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SubmitSurvey m ->
            ( m, makeRequest m )

        GotResponse submitResult ->
            ( { model | submitResult = submitResult }, Cmd.none )


updateCurrentAnswer : Answer -> Model -> Model
updateCurrentAnswer newAnswer model =
    let
        survey =
            model.survey

        currentQuestion =
            model.survey.current
    in
    { model | survey = { survey | current = { currentQuestion | answer = newAnswer } } }


mutation : Model -> SelectionSet Scalar.Id RootMutation
mutation model =
    Mutation.createSurvey { data = surveyToInput model.currentTime model.survey } SurveyDocument.id_


makeRequest : Model -> Cmd Msg
makeRequest model =
    mutation model
        |> Graphql.Http.mutationRequest "https://graphql.fauna.com/graphql"
        |> Graphql.Http.withHeader "Authorization" "Bearer fnAD68qBMJACB2CL8qqCRoQpRcviQQSbl27YO3pj"
        |> Graphql.Http.send (RemoteData.fromResult >> GotResponse)



-- VIEW


view : Model -> Html Msg
view model =
    let
        mainContent =
            case model.submitResult of
                Success _ ->
                    div
                        [ style "font-family" "'Comfortaa', cursive"
                        , class "flex flex-col items-center text-green-600 text-6xl font-semibold items-center justify-center transition-all duration-1000 ease-out"
                        ]
                        [ p [] [ text "Thank you for" ]
                        , p [] [ text "submitting!" ]
                        , p [ class "transform scale-150 mt-12" ] [ text "✅" ]
                        ]

                _ ->
                    div [ class "sm:w-10/12 lg:w-8/12 h-full p-12 lg:p-6 mt-12 mb-12 bg-white shadow-md rounded flex flex-col justify-between items-center sm:rounded-xl lg:rounded-md" ]
                        [ div [ class "flex flex-col items-center" ]
                            [ span [ class "sm:text-xl md:text-4xl text-gray-500 mb-5" ] [ currentQuestionNoText model.survey ]
                            , p [ class "text-6xl lg:text-2xl inline-block text-center" ] [ text model.survey.current.text ]
                            ]
                        , div [ class "text-5xl lg:text-3xl flex flex-col items-center w-full" ]
                            [ viewAnswer model.survey ]
                        , div [ class "flex flex-row w-full sm:text-2xl md:text-4x" ]
                            (questionControlButtons model)
                        ]
    in
    main_ [ class "w-full h-full flex flex-col items-center justify-center text-xl relative" ]
        [ mainContent
        , p [ class "text-gray-600 mb-2 absolute bottom-0" ]
            [ text "Made by "
            , a [ class "underline", href "https://twitter.com/kajetansw", target "_blank" ] [ text "@kajetansw" ]
            , text " with Elm, TailwindCSS and FaunaDB. "
            , a [ class "underline", href "https://github.com/kajetansw/elm-survey-app", target "_blank" ] [ text "Source code" ]
            ]
        ]


questionControlButtons : Model -> List (Html Msg)
questionControlButtons model =
    let
        defaultButtonStyles =
            "text-black font-bold py-8 lg:py-2 px-4 lg:px-1 text-3xl lg:text-xl w-3/6 ml-4 rounded "

        nextOrSubmitButton =
            if isLastQuestion model.survey then
                button
                    [ onClick (SubmitSurvey model)
                    , class ("bg-green-400 " ++ defaultButtonStyles)
                    ]
                    [ text "Submit ✔" ]

            else
                button
                    [ onClick NextQuestion
                    , class ("bg-yellow-500 " ++ defaultButtonStyles ++ buttonDisabledStyle model)
                    , disabled (isErr (validateAnswer model.survey.current.answer))
                    ]
                    [ text "Next ▶" ]

        previousOrEmpty =
            if isFirstQuestion model.survey then
                div [ class "w-3/6" ] []

            else
                button
                    [ onClick PreviousQuestion
                    , class ("bg-yellow-500 " ++ defaultButtonStyles)
                    ]
                    [ text "◀ Previous" ]
    in
    [ previousOrEmpty
    , nextOrSubmitButton
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
                            label [ class "flex items-center mb-10 lg:mb-2" ]
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
                [ class "form-select w-10/12 mt-1 text-5xl lg:text-2xl"
                , onChange (\val -> SelectAnswerChanged val)
                ]
                (List.map optionToHtml (answer.default :: answer.options))

        TextAnswer txt ->
            textarea
                [ class "form-textarea w-full text-5xl lg:text-3xl"
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


buttonDisabledStyle : Model -> String
buttonDisabledStyle model =
    let
        toBeDisabled : Bool
        toBeDisabled =
            isErr (validateAnswer model.survey.current.answer) || List.isEmpty model.survey.next
    in
    if toBeDisabled then
        "bg-opacity-25 text-opacity-25 pointer-events-none"

    else
        ""


isErr : Result a b -> Bool
isErr r =
    case r of
        Ok _ ->
            False

        Err _ ->
            True



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
