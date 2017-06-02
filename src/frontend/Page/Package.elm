module Page.Package exposing (..)

import Component.PackageDocs as PDocs
import Component.PackageSidebar as PkgNav
import Html exposing (..)
import Html.Attributes exposing (..)
import Page.Context as Ctx
import Route
import Task


-- MODEL


type alias Model =
    { moduleDocs : PDocs.Model
    , pkgNav : PkgNav.Model
    }



-- INIT


init : Maybe String -> Ctx.ElmPackage -> ( Model, Cmd Msg )
init moduleName package =
    let
        ( moduleDocs, moduleCmd ) =
            PDocs.init package moduleName

        ( pkgNav, navCmd ) =
            PkgNav.init moduleName package.github
    in
    ( Model moduleDocs pkgNav
    , Cmd.batch
        [ Cmd.map UpdateDocs moduleCmd
        , Cmd.map UpdateNav navCmd
        ]
    )



-- UPDATE


type Msg
    = UpdateDocs PDocs.Msg
    | UpdateNav PkgNav.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateDocs docsMsg ->
            let
                ( newDocs, fx ) =
                    PDocs.update docsMsg model.moduleDocs
            in
            ( { model | moduleDocs = newDocs }
            , Cmd.map UpdateDocs fx
            )

        UpdateNav navMsg ->
            let
                ( newPkgNav, fx ) =
                    PkgNav.update navMsg model.pkgNav
            in
            ( { model | pkgNav = newPkgNav }
            , Cmd.map UpdateNav fx
            )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ Html.map UpdateDocs (PDocs.view model.moduleDocs)
        , Html.map UpdateNav (PkgNav.view model.pkgNav)
        ]
