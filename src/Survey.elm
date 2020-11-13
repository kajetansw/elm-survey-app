module Survey exposing (..)

import Html exposing (Html, div, text)



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
    , picked : String
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
pickSelectAnswer { options, picked } label =
    if List.member label options then
        { options = options, picked = label }

    else
        { options = options, picked = picked }


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



-- STUBS


question1 : Question
question1 =
    { text = "What's your daily front-end stack?"
    , answer =
        CheckboxAnswer
            [ { text = "JavaScript", value = False }
            , { text = "TypeScript", value = False }
            , { text = "Elm", value = False }
            , { text = "Angular", value = False }
            , { text = "React", value = False }
            , { text = "Vue", value = False }
            , { text = "PureScript", value = False }
            ]
    }


question2 : Question
question2 =
    { text = "Have you ever tried Elm?"
    , answer =
        SelectAnswer
            { options = [ "Yes", "No" ]
            , picked = "Yes"
            }
    }


question3 : Question
question3 =
    { text = "If so, for how many years?"
    , answer =
        SelectAnswer
            { options = [ "0 - 1", "2 - 3", "3 - 5", "5 - 8" ]
            , picked = "0 - 1"
            }
    }


question4 : Question
question4 =
    { text = "If no, will you give it a try?"
    , answer =
        SelectAnswer
            { options = [ "Yes", "No" ]
            , picked = "Yes"
            }
    }


question5 : Question
question5 =
    { text = "How did you enjoy the 'Why Elm is a delightful language to learn FP?' talk?"
    , answer =
        RateAnswer 5 Nothing
    }


question6 : Question
question6 =
    { text = "Write down any feedback you have!"
    , answer = TextAnswer ""
    }
