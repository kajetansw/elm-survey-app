module Survey.Serialize exposing (..)

import Survey.Core exposing (..)
import SurveyAPI.InputObject exposing (..)
import SurveyAPI.Scalar exposing (..)


questionToInput : Question -> QuestionInput
questionToInput q =
    let
        answer : String
        answer =
            case q.answer of
                CheckboxAnswer ls ->
                    let
                        answered =
                            List.filter (\x -> x.value == True) ls
                    in
                    String.join "," (List.map (\x -> x.text) answered)

                SelectAnswer sa ->
                    case sa.picked of
                        Just v ->
                            v

                        Nothing ->
                            ""

                TextAnswer txt ->
                    txt

                RateAnswer range rate ->
                    case rate of
                        Just r ->
                            String.fromInt r ++ "/" ++ String.fromInt range

                        Nothing ->
                            ""
    in
    { text = q.text, answer = answer }


surveyToInput : Survey -> SurveyInput
surveyToInput s =
    let
        questions : List (Maybe QuestionInput)
        questions =
            List.map (\q -> Just (questionToInput q)) (toList s)
    in
    { date = Date "2020-06-06", questions = questions }

