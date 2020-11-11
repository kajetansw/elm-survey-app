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
    | IntegerAnswer (Maybe Int)


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
        SelectAnswer
            { options = [ "5 (it was great)", "4", "3", "2", "1 (it was awful)" ]
            , picked = "5 (it was great)"
            }
    }


question6 : Question
question6 =
    { text = "Do you have any feedback?"
    , answer = TextAnswer ""
    }
