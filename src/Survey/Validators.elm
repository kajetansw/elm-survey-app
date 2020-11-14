module Survey.Validators exposing (validateAnswer)

import Survey.Core exposing (..)


validateAnswer : Answer -> Result String Answer
validateAnswer a =
    case a of
        CheckboxAnswer ls ->
            validateCheckboxAnswer ls

        SelectAnswer sa ->
            validateSelectAnswer sa

        TextAnswer txt ->
            validateTextAnswer txt

        RateAnswer range maybeRate ->
            validateRateAnswer range maybeRate


validateCheckboxAnswer : List (Labeled Bool) -> Result String Answer
validateCheckboxAnswer ls =
    let
        allAnswerValues =
            List.map (\x -> x.value) ls
    in
    if List.all (\x -> x == False) allAnswerValues then
        Err "At least one answer has to be picked."

    else
        Ok (CheckboxAnswer ls)


validateSelectAnswer : SelectAnswer_ -> Result String Answer
validateSelectAnswer sa =
    case sa.picked of
        Just _ ->
            Ok (SelectAnswer sa)

        Nothing ->
            Err "One of available answers has to be picked."


validateTextAnswer : String -> Result String Answer
validateTextAnswer txt =
    Ok (TextAnswer txt)


validateRateAnswer : Int -> Maybe Int -> Result String Answer
validateRateAnswer range rate =
    case rate of
        Just _ ->
            Ok (RateAnswer range rate)

        Nothing ->
            Err "Rate must be provided."
