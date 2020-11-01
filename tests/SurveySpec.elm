module SurveySpec exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Survey as S exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Survey module"
        [ describe "nextQuestion"
            [ test "should switch to next question correctly" <|
                \_ ->
                    let
                        inputSurvey : Survey
                        inputSurvey =
                            { previous = [ testQuestion1 ]
                            , current = testQuestion2
                            , next = [ testQuestion3 ]
                            }

                        outputSurvey : Survey
                        outputSurvey =
                            { previous = [ testQuestion1, testQuestion2 ]
                            , current = testQuestion3
                            , next = []
                            }
                    in
                    Expect.equal (S.nextQuestion inputSurvey) outputSurvey
            , test "should do nothing when trying to switch to next question if there are no further questions" <|
                \_ ->
                    let
                        testSurvey : Survey
                        testSurvey =
                            { previous = [ testQuestion1, testQuestion2 ]
                            , current = testQuestion3
                            , next = []
                            }
                    in
                    Expect.equal (S.nextQuestion testSurvey) testSurvey
            ]
        , describe "previousQuestion"
            [ test "should switch to previous question correctly" <|
                \_ ->
                    let
                        inputSurvey : Survey
                        inputSurvey =
                            { previous = [ testQuestion1 ]
                            , current = testQuestion2
                            , next = [ testQuestion3 ]
                            }

                        outputSurvey : Survey
                        outputSurvey =
                            { previous = []
                            , current = testQuestion1
                            , next = [ testQuestion2, testQuestion3 ]
                            }
                    in
                    Expect.equal (S.previousQuestion inputSurvey) outputSurvey
            , test "should do nothing when trying to switch to previous question if there are no previous questions" <|
                \_ ->
                    let
                        testSurvey : Survey
                        testSurvey =
                            { previous = []
                            , current = testQuestion1
                            , next = [ testQuestion2, testQuestion3 ]
                            }
                    in
                    Expect.equal (S.previousQuestion testSurvey) testSurvey
            ]
        ]


testQuestion1 : Question
testQuestion1 =
    Question
        { text = "TestQuestion1"
        , answer =
            CheckboxAnswer
                [ { text = "A1", value = False }
                , { text = "A2", value = False }
                , { text = "A3", value = False }
                ]
        }


testQuestion2 : Question
testQuestion2 =
    Question
        { text = "TestQuestion2"
        , answer = YesNoAnswer False
        }


testQuestion3 : Question
testQuestion3 =
    Question
        { text = "TestQuestion3"
        , answer =
            TextAnswer
                { text = "TestAnswer"
                , value = "TestAnswer"
                }
        }


testQuestion4 : Question
testQuestion4 =
    Question
        { text = "TestQuestion3"
        , answer =
            IntegerAnswer
                { text = "TestAnswer"
                , value = 1
                }
        }
