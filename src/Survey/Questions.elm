module Survey.Questions exposing (..)

import Survey.Core exposing (..)


question1 : Question
question1 =
    { text = "What's your primary front-end stack?"
    , answer =
        CheckboxAnswer
            [ { text = "JavaScript", value = False }
            , { text = "TypeScript", value = False }
            , { text = "Angular", value = False }
            , { text = "React", value = False }
            , { text = "Vue", value = False }
            , { text = "Elm", value = False }
            , { text = "PureScript", value = False }
            , { text = "ReScript/Reason", value = False }
            , { text = "ScalaJS", value = False }
            , { text = "Other", value = False }
            ]
    }


question2 : Question
question2 =
    { text = "Have you ever tried Elm?"
    , answer =
        SelectAnswer
            { options = [ "Yes", "No" ]
            , picked = Nothing
            , default = "(Select your answer)"
            }
    }


question3 : Question
question3 =
    { text = "If no, will you give it a try?"
    , answer =
        SelectAnswer
            { options = [ "Definitely!", "Maybe", "Meh..." ]
            , picked = Nothing
            , default = "(Select your answer)"
            }
    }


question4 : Question
question4 =
    { text = "How did you enjoy the 'Why Elm is a delightful language to learn FP?' talk?"
    , answer =
        RateAnswer 5 Nothing
    }


question5 : Question
question5 =
    { text = "Write down any feedback you have!"
    , answer = TextAnswer ""
    }
