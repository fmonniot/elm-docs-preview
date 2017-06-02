module Reactor exposing (..)

import Component.Header as Header
import Html exposing (..)
import Html.Attributes
import Http
import Json.Decode as Json
import Navigation exposing (..)
import Page.Context as Ctx
import Page.Package as Pkg
import Regex
import Route exposing (R)
import UrlParser as Url


-- WIRES


main : Program Never Model Msg
main =
    Navigation.program loc2msg
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { route : R
    , content : Content
    }


type Content
    = Loading
    | Failed Http.Error
    | Loaded
        { package : Ctx.ElmPackage
        , model : Pkg.Model
        , header : Header.Model
        }



-- INIT


init : Location -> ( Model, Cmd Msg )
init loc =
    let
        route =
            Url.parseHash Route.parser loc |> Maybe.withDefault Route.Readme
    in
    { route = route, content = Loading } ! [ loadElmPackage ]



-- UPDATE


type Msg
    = PkgMsg Pkg.Msg
    | LoadElmPackage (Result Http.Error Ctx.ElmPackage)
    | UrlChange Location


loc2msg : Location -> Msg
loc2msg =
    UrlChange


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange loc ->
            let
                route =
                    Url.parseHash Route.parser loc |> Maybe.withDefault Route.Readme

                moduleName =
                    case route of
                        Route.Module s ->
                            Just s

                        _ ->
                            Nothing
            in
            case model.content of
                Loaded mo ->
                    let
                        ( initModel, initCmd ) =
                            Pkg.init moduleName mo.package

                        ( header, headerCmd ) =
                            Header.init mo.package route

                        m =
                            { mo | package = mo.package, model = initModel, header = header }
                    in
                    { model | content = Loaded m, route = route }
                        ! [ Cmd.map PkgMsg initCmd
                          , headerCmd
                          ]

                _ ->
                    { model | route = route } ! []

        PkgMsg pkgMsg ->
            case model.content of
                Loaded thing ->
                    let
                        ( updatedModel, msg ) =
                            Pkg.update pkgMsg thing.model
                    in
                    { model | content = Loaded { thing | model = updatedModel } }
                        ! [ Cmd.map PkgMsg msg ]

                _ ->
                    model ! []

        LoadElmPackage (Err error) ->
            ( { model | content = Failed error }, Cmd.none )

        LoadElmPackage (Ok elmPackage) ->
            let
                moduleName =
                    case model.route of
                        Route.Module s ->
                            Just s

                        _ ->
                            Nothing

                ( initModel, initCmd ) =
                    Pkg.init moduleName elmPackage

                ( header, headerCmd ) =
                    Header.init elmPackage model.route

                m =
                    { package = elmPackage, model = initModel, header = header }
            in
            { model | content = Loaded m }
                ! [ Cmd.map PkgMsg initCmd
                  , headerCmd
                  ]



-- EFFECTS


loadElmPackage : Cmd Msg
loadElmPackage =
    Http.send LoadElmPackage (Http.get "/elm-package.json" Ctx.decodeElmPackage)



-- VIEW


view : Model -> Html Msg
view model =
    case model.content of
        Loaded content ->
            div []
                [ Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "/assets/highlight/styles/default.css?1495720958" ] []
                , Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "/assets/style.css?1495720958" ] []
                , Html.node "script" [ Html.Attributes.src "/assets/highlight/highlight.pack.js?1495720958" ] []
                , Header.view content.header
                    [ Html.map PkgMsg <| Pkg.view content.model
                    ]
                ]

        Loading ->
            div [] [ text "Loading project information" ]

        Failed err ->
            div [] [ text <| "Cannot load info.json because" ++ toString err ]
