module Component.Header exposing (..)

import Docs.Version as Version
import Html exposing (..)
import Html.Attributes exposing (..)
import Page.Context exposing (ElmPackage, pathToModule)
import Route exposing (..)
import Utils.Path as Path exposing ((</>))


-- MODEL


type alias Model =
    { package : ElmPackage
    , route : R
    }


init : ElmPackage -> R -> ( Model, Cmd msg )
init package route =
    ( Model package route
    , Cmd.none
    )



-- UPDATE


update : msg -> Model -> ( Model, Cmd msg )
update msg model =
    ( model, Cmd.none )



-- VIEW


(=>) =
    (,)


view : Model -> List (Html msg) -> Html msg
view model contents =
    div []
        [ center "#eeeeee" [ headerLinks model ]
        , div [ class "center" ] contents
        , div [ class "footer" ]
            [ text "All code for this site is open source and written in Elm. "
            , a [ class "grey-link", href "https://github.com/elm-lang/package.elm-lang.org/" ] [ text "Check it out" ]
            , text "! — © 2012-2016 Evan Czaplicki"
            ]
        ]


center color kids =
    div [ style [ "background-color" => color ] ]
        [ div [ class "center" ] kids
        ]



-- VIEW ROUTE LINKS


headerLinks model =
    h1 [ class "header" ] <|
        a [ href "#", style [ "text-decoration" => "none" ] ] [ logo ]
            :: makeBreadcumb model



-- helpers


spacey token =
    span [ class "spacey-char" ] [ text token ]


logo =
    div
        [ style
            [ "display" => "-webkit-display"
            , "display" => "-ms-flexbox"
            , "display" => "flex"
            ]
        ]
        [ img
            [ src "/assets/elm_logo.svg"
            , style
                [ "height" => "30px"
                , "vertical-align" => "bottom"
                , "padding-right" => "8px"
                ]
            ]
            []
        , div
            [ style
                [ "color" => "black"
                ]
            ]
            [ div
                [ style
                    [ "line-height" => "20px"
                    ]
                ]
                [ text "elm" ]
            , div
                [ style
                    [ "line-height" => "10px"
                    , "font-size" => "0.5em"
                    ]
                ]
                [ text "packages" ]
            ]
        ]


headerLink url words =
    a [ href url, style [ "color" => "#333333" ] ]
        [ text words ]



-- route unrolling
-- TODO Rename to correct name


makeBreadcumb : Model -> List (Html msg)
makeBreadcumb model =
    let
        user =
            model.package.github.user

        project =
            model.package.github.project

        version =
            model.package.version

        moduleName =
            case model.route of
                Module moduleName ->
                    [ spacey "/"
                    , headerLink (pathToModule moduleName) moduleName
                    ]

                _ ->
                    []
    in
    [ headerLink ("http" </> user) user
    , spacey "/"
    , headerLink ("https://github.com" </> user </> project) project
    , spacey "/"
    , text ("master (" ++ version ++ ")")
    ]
        ++ moduleName
