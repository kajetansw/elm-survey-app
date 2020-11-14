module Survey.Core exposing (..)

-- TYPES


type alias Survey =
    { previous : List Question
    , current : Question
    , next : List Question
    }


type alias Question =
    { text : String
    , answer : Answer
    }


type Answer
    = CheckboxAnswer (List (Labeled Bool))
    | SelectAnswer SelectAnswer_
    | TextAnswer String
    | RateAnswer Int (Maybe Int)


type alias AnchorURL =
    String


type alias AnchorText =
    String


type alias SelectAnswer_ =
    { options : List String
    , picked : Maybe String
    , default : String
    }


type alias Labeled v =
    { text : String
    , value : v
    }



-- FUNCTIONS


pickCheckboxAnswer : String -> Bool -> List (Labeled Bool) -> List (Labeled Bool)
pickCheckboxAnswer text value labeledValues =
    let
        updateLabeled : Labeled Bool -> List (Labeled Bool) -> List (Labeled Bool)
        updateLabeled curr acc =
            if curr.text == text then
                { curr | value = value } :: acc

            else
                curr :: acc
    in
    List.foldr updateLabeled [] labeledValues


pickSelectAnswer : SelectAnswer_ -> String -> SelectAnswer_
pickSelectAnswer sAnswer label =
    if List.member label sAnswer.options then
        { sAnswer | picked = Just label }

    else
        sAnswer


nextQuestion : Survey -> Survey
nextQuestion { previous, current, next } =
    case next of
        [] ->
            Survey previous current next

        newCurrent :: rest ->
            Survey (List.append previous [ current ]) newCurrent rest


previousQuestion : Survey -> Survey
previousQuestion { previous, current, next } =
    case List.reverse previous of
        [] ->
            Survey previous current next

        newCurrent :: rest ->
            Survey (List.reverse rest) newCurrent (List.append [ current ] next)


currentQuestionNumber : Survey -> Int
currentQuestionNumber s =
    List.length s.previous + 1


surveyLength : Survey -> Int
surveyLength s =
    currentQuestionNumber s + List.length s.next
