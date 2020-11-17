-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module SurveyAPI.Object.Question exposing (..)

import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode
import SurveyAPI.InputObject
import SurveyAPI.Interface
import SurveyAPI.Object
import SurveyAPI.Scalar
import SurveyAPI.ScalarCodecs
import SurveyAPI.Union


text : SelectionSet String SurveyAPI.Object.Question
text =
    Object.selectionForField "String" "text" [] Decode.string


answer : SelectionSet String SurveyAPI.Object.Question
answer =
    Object.selectionForField "String" "answer" [] Decode.string