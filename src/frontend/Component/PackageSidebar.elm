module Component.PackageSidebar exposing (..)

import Dict
import Docs.Entry as Entry
import Docs.Package as Docs
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Page.Context as Ctx
import Set
import String
import Utils.Path as Path exposing ((</>))


type Model
    = Loading
    | Failed Http.Error
    | Success
        { moduleName : ModuleName
        , searchDict : SearchDict
        , query : String
        , github : Ctx.GithubRepo
        }


type alias SearchDict =
    Dict.Dict String (List LinkInfo)


type alias LinkInfo =
    { name : String
    , owner : String
    }


type alias ModuleName =
    Maybe String



-- INIT


init : ModuleName -> Ctx.GithubRepo -> ( Model, Cmd Msg )
init moduleName github =
    ( Loading
    , loadDocs moduleName github
    )



-- UPDATE


type Msg
    = LoadDocs ModuleName Ctx.GithubRepo (Result Http.Error SearchDict)
    | Query String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Query query ->
            flip (,) Cmd.none <|
                case model of
                    Success facts ->
                        Success { facts | query = query }

                    Loading ->
                        model

                    Failed err ->
                        model

        LoadDocs _ _ (Err httpError) ->
            ( Failed httpError
            , Cmd.none
            )

        LoadDocs moduleName github (Ok searchDict) ->
            ( Success
                { moduleName = moduleName
                , searchDict = searchDict
                , query = ""
                , github = github
                }
            , Cmd.none
            )



-- EFFECTS


loadDocs : ModuleName -> Ctx.GithubRepo -> Cmd Msg
loadDocs moduleName github =
    Http.send (LoadDocs moduleName github << Result.map toSearchDict) Ctx.getDocs


toSearchDict : Docs.Package -> SearchDict
toSearchDict pkg =
    Dict.map toLinkInfo pkg


toLinkInfo : String -> Docs.Module -> List LinkInfo
toLinkInfo _ modul =
    let
        entryNames =
            Dict.keys modul.entries

        nameSet =
            Set.fromList entryNames

        tagInfo =
            Dict.values modul.entries
                |> List.concatMap (gatherTagInfo nameSet)

        topLevelInfo =
            List.map (\name -> LinkInfo name name) entryNames
    in
    tagInfo ++ topLevelInfo


gatherTagInfo : Set.Set String -> Entry.Model t -> List { name : String, owner : String }
gatherTagInfo topLevelNames entry =
    let
        toNamePair { tag } =
            if Set.member tag topLevelNames then
                Nothing
            else
                Just (LinkInfo tag entry.name)
    in
    case entry.info of
        Entry.Union { tags } ->
            List.filterMap toNamePair tags

        _ ->
            []



-- VIEW


(=>) =
    (,)


view : Model -> Html Msg
view model =
    div [ class "pkg-nav" ] <|
        case model of
            Loading ->
                [ p [] [ text "Loading..." ]
                ]

            Failed httpError ->
                [ p [] [ text "Problem loading!" ]
                , p [] [ text (toString httpError) ]
                ]

            Success { moduleName, query, searchDict, github } ->
                [ moduleLink moduleName Nothing
                , br [] []
                , githubLink github
                , h2 [] [ text "Module Docs" ]
                , input
                    [ placeholder "Search"
                    , value query
                    , onInput Query
                    ]
                    []
                , viewSearchDict moduleName query searchDict
                ]


viewSearchDict : ModuleName -> String -> SearchDict -> Html msg
viewSearchDict currentModuleName query searchDict =
    if String.isEmpty query then
        ul [] (List.map (li [] << singleton << moduleLink currentModuleName << Just) (Dict.keys searchDict))
    else
        let
            lowerQuery =
                String.toLower query

            containsQuery value =
                String.contains lowerQuery (String.toLower value)

            searchResults =
                searchDict
                    |> Dict.map (\_ values -> List.filter (.name >> containsQuery) values)
                    |> Dict.filter (\key values -> not (List.isEmpty values) || containsQuery key)
                    |> Dict.toList
        in
        ul [] (List.map (viewModuleLinks currentModuleName) searchResults)


viewModuleLinks : ModuleName -> ( String, List LinkInfo ) -> Html msg
viewModuleLinks package ( name, values ) =
    li
        [ class "pkg-nav-search-chunk" ]
        [ moduleLink package (Just name)
        , ul [] (List.map (valueLink name) values)
        ]


githubLink : Ctx.GithubRepo -> Html msg
githubLink github =
    a
        [ class "pkg-nav-module"
        , href ("https://github.com" </> github.user </> github.project)
        ]
        [ text "Browse source" ]


moduleLink : ModuleName -> ModuleName -> Html msg
moduleLink currentModule name =
    let
        visibleName =
            Maybe.withDefault "README" name

        url =
            Ctx.pathToModule (Maybe.withDefault "" (Maybe.map Path.hyphenate name))

        visibleText =
            if currentModule == name then
                span [ style [ "font-weight" => "bold", "text-decoration" => "underline" ] ] [ text visibleName ]
            else
                text visibleName
    in
    a [ class "pkg-nav-module", href url ] [ visibleText ]


valueLink : String -> LinkInfo -> Html msg
valueLink moduleName { name, owner } =
    let
        url =
            Ctx.pathTo (Path.hyphenate moduleName) ++ "#" ++ owner
    in
    li
        [ class "pkg-nav-value"
        ]
        [ a [ href url ] [ text name ]
        ]


singleton : a -> List a
singleton x =
    [ x ]
