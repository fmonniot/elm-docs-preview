module Component.Header exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)

import Docs.Version as Version
import Route exposing (..)
import Utils.Path as Path exposing ((</>))



-- MODEL


type alias Model =
    { route : Route
    }


init : Route -> (Model, Cmd msg)
init route =
  ( Model route
  , Cmd.none
  )



-- UPDATE


update : msg -> Model -> (Model, Cmd msg)
update msg model =
  (model, Cmd.none)



-- VIEW


(=>) = (,)


view : Model -> List (Html msg) -> Html msg
view model contents =
  div []
    [ center "#eeeeee" [ headerLinks model ]
    , center "#60B5CC" (versionWarning model)
    , div [ class "center" ] contents
    , div [class "footer"]
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
    a [href "/", style ["text-decoration" => "none"]] [logo]
    :: unrollRoute model.route


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

unrollRoute : Route -> List (Html msg)
unrollRoute route =
  case route of
    Help ->
        [ text "help" ]

    Packages userRoute ->
        maybe unrollUserRoute userRoute


maybe : (a -> List (Html msg)) -> Maybe a -> List (Html msg)
maybe unroll maybeRoute =
  case maybeRoute of
    Nothing ->
        []

    Just route ->
        unroll route


unrollUserRoute : UserRoute -> List (Html msg)
unrollUserRoute (User user packageRoute) =
    headerLink ("https://github.com" </> user) user
    :: maybe (unrollPackageRoute user) packageRoute


unrollPackageRoute : String -> PackageRoute -> List (Html msg)
unrollPackageRoute user (Package pkg versionRoute) =
    spacey "/"
    :: headerLink ("/packages" </> user </> pkg) pkg
    :: maybe (unrollVersionRoute user pkg) versionRoute


unrollVersionRoute : String -> String -> VersionRoute -> List (Html msg)
unrollVersionRoute user pkg (Version vsn _ moduleRoute) =
  spacey "/"
  :: headerLink ("/packages" </> user </> pkg </> vsn) vsn
  :: maybe (unrollModuleeRoute user pkg vsn) moduleRoute


unrollModuleeRoute : String -> String -> String -> String -> List (Html msg)
unrollModuleeRoute user pkg vsn name =
  [ spacey "/"
  , headerLink ("/packages" </> user </> pkg </> vsn </> Path.hyphenate name) name
  ]



-- version warnings


versionWarning : Model -> List (Html msg)
versionWarning model =
  let
    warning =
      case model.route of
        Packages (Just (User user (Just (Package project (Just (Version vsn allVersions maybeName)))))) ->
            case Version.realMax vsn allVersions of
              Nothing ->
                []

              Just maxVersion ->
                let
                  moduleName =
                    Maybe.withDefault "" (Maybe.map Path.hyphenate maybeName)
                in
                  [ p [ class "version-warning" ]
                      [ text "Warning! The latest version of this package is "
                      , a [ href ("/packages" </> user </> project </> "latest" </> moduleName) ]
                          [ text maxVersion ]
                      ]
                  ]

        _ ->
          []
  in
    [ div [ class "header-underbar" ] warning ]
