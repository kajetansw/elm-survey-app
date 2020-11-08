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
    | RadioAnswer RadioAnswer_
    | TextAnswer String
    | IntegerAnswer (Maybe Int)
    | YesNoAnswer Bool
    | GoToURLButton AnchorURL AnchorText


type alias AnchorURL =
    String


type alias AnchorText =
    String


type alias RadioAnswer_ =
    { labels : List Label
    , picked : Maybe Label
    }


type alias Labeled v =
    { text : String
    , value : v
    }


type Label
    = Label String



-- FUNCTIONS


pickRadioAnswer : RadioAnswer_ -> Label -> RadioAnswer_
pickRadioAnswer { labels, picked } label =
    if List.member label labels then
        { labels = labels, picked = Just label }

    else
        { labels = labels, picked = Nothing }


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


currentQuestionNo : Survey -> Int
currentQuestionNo s =
    List.length s.previous + 1


surveyLength : Survey -> Int
surveyLength s =
    currentQuestionNo s + List.length s.next



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
    , answer = YesNoAnswer False
    }


question3 : Question
question3 =
    { text = "If so, for how many years?"
    , answer =
        RadioAnswer
            { labels = [ Label "0 - 1", Label "2 - 3", Label "3 - 5", Label "5 - 8" ]
            , picked = Nothing
            }
    }


question4 : Question
question4 =
    { text = "If no, will you give it a try?"
    , answer = YesNoAnswer False
    }


question5 : Question
question5 =
    { text = "Did you enjoy the talk? :)"
    , answer = YesNoAnswer False
    }


question6 : Question
question6 =
    { text = "If so, give me a follow for more content!"
    , answer = GoToURLButton "http://twitter.com/kajetansw" "Twitter @kajetansw"
    }


question7 : Question
question7 =
    { text = "If you have any, please give me some feedback (optional)"
    , answer = TextAnswer ""
    }
