module Reactor exposing (..)

import Html exposing (..)
import Html.Attributes
import Http
import Json.Decode as Json
import Page.Context as Ctx
import Page.Package as Pkg


-- WIRES


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type Model
    = Loading
    | Failed Http.Error
    | Success
        { context : Ctx.VersionContext
        , model : Pkg.Model
        }



-- INIT


init : ( Model, Cmd Msg )
init =
    ( Loading, loadElmPackage )



-- UPDATE


type Msg
    = PkgMsg Pkg.Msg
    | LoadElmPackage (Result Http.Error Ctx.VersionContext)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PkgMsg pkgMsg ->
            case model of
                Success thing ->
                    let
                        ( updatedModel, fx ) =
                            Pkg.update pkgMsg thing.model
                    in
                    ( Success { thing | model = updatedModel }, Cmd.map PkgMsg fx )

                _ ->
                    model ! []

        LoadElmPackage (Err error) ->
            ( Failed error, Cmd.none )

        LoadElmPackage (Ok context) ->
            let
                ( initModel, initCmd ) =
                    Pkg.init context

                m =
                    { context = context, model = initModel }
            in
            ( Success m, Cmd.map PkgMsg initCmd )



-- EFFECTS


loadElmPackage : Cmd Msg
loadElmPackage =
    Http.send LoadElmPackage (Http.get "/elm-package.json" decodeVersionContext)


decodeVersionContext : Json.Decoder Ctx.VersionContext
decodeVersionContext =
    let
        decoder user repo version =
            Ctx.VersionContext user repo version [] Nothing
    in
    Json.map3 decoder
        (Json.at [ "github", "user" ] Json.string)
        (Json.at [ "github", "repo" ] Json.string)
        (Json.at [ "version" ] Json.string)



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        Success thing ->
            div []
                [ Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "/assets/highlight/styles/default.css?1495720958" ] []
                , Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "/assets/style.css?1495720958" ] []
                , Html.node "script" [ Html.Attributes.src "/assets/highlight/highlight.pack.js?1495720958" ] []
                , Html.map PkgMsg <| Pkg.view thing.model
                ]

        Loading ->
            div [] [ text "Loading project information" ]

        Failed err ->
            div [] [ text <| "Cannot load info.json because" ++ toString err ]
