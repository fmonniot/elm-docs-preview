module Reactor exposing (..)

import Html exposing (..)
import Html.Attributes
import Page.Package exposing (..)


ini =
    { user = "krisajenkins"
    , project = "remotedata"
    , version = "4.3.0"
    , allVersions = [ "4.3.0", "4.2.1", "4.2.0", "4.1.0", "4.0.1", "4.0.0", "3.0.0", "2.4.0", "2.3.0", "2.2.1", "2.2.0", "2.1.1", "2.1.0", "2.0.0", "1.0.1", "1.0.0" ]
    , moduleName = Nothing
    }


v : Model -> Html Msg
v model =
    div []
        [ Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "/assets/highlight/styles/default.css?1495720958" ] []
        , Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "/assets/style.css?1495720958" ] []
        , Html.node "script" [ Html.Attributes.src "/assets/highlight/highlight.pack.js?1495720958" ] []
        , view model
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = init ini
        , view = v
        , update = update
        , subscriptions = \_ -> Sub.none
        }
