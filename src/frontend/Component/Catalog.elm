module Component.Catalog exposing (..)

import Html exposing (..)
import Html.Attributes exposing (autofocus, class, href, placeholder, style, value)
import Html.Events exposing (..)
import Http
import Json.Decode as Json
import Set
import String
import Task

import Docs.Summary as Summary
import Docs.Version as Vsn
import Utils.FluidList as FluidList
import Utils.Markdown as Markdown



-- MODEL


type Model
    = Loading
    | Failed Http.Error
    | Success
        { summaries : List Summary.Summary
        , oldSummaries : List Summary.Summary
        , query : String
        }



-- INIT


init : (Model, Cmd Msg)
init =
  ( Loading
  , getPackageInfo
  )



-- UPDATE


type Msg
    = GetInfo (Result Http.Error (List Summary.Summary, List String))
    | Query String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GetInfo (Err httpError) ->
        ( Failed httpError
        , Cmd.none
        )

    GetInfo (Ok (allSummaries, updatedPkgs)) ->
      let
        updatedSet =
          Set.fromList updatedPkgs

        (summaries, oldSummaries) =
          List.partition (\{name} -> Set.member name updatedSet) allSummaries
      in
        ( Success
            { summaries = summaries
            , oldSummaries = oldSummaries
            , query = ""
            }
        , Cmd.none
        )

    Query query ->
      flip (,) Cmd.none <|
        case model of
          Success facts ->
              Success { facts | query = query }

          Loading ->
              model

          Failed err ->
              model



searchFor : String -> List Summary.Summary -> List Summary.Summary
searchFor query summaries =
  let
    queryTerms =
      String.words (String.toLower query)

    matchesQueryTerms {name,summary} =
      let
        lowerName =
          String.toLower name

        lowerSummary =
          String.toLower summary

        findTerm term =
          String.contains term lowerName
          || String.contains term lowerSummary
      in
        List.all findTerm queryTerms
  in
    List.filter matchesQueryTerms summaries



-- EFFECTS


getPackageInfo : Cmd Msg
getPackageInfo =
  let
    getAll =
      Http.get "/all-packages" Summary.decoder

    getNew =
      Http.get "/new-packages" (Json.list Json.string)
  in
    Task.attempt GetInfo <|
      Task.map2 (,) (Http.toTask getAll) (Http.toTask getNew)



-- VIEW


(=>) = (,)


view : Model -> Html Msg
view model =
  div [class "catalog"] <|
    case model of
      Loading ->
          [ p [] [text "Loading..."]
          ]

      Failed httpError ->
          [ p [] [text "Problem loading package list!"]
          , p [] [text (toString httpError)]
          ]

      Success {summaries, oldSummaries, query} ->
          [ input
              [ placeholder "Search"
              , value query
              , onInput Query
              , autofocus True
              ]
              []
          , div [] (List.map viewSummary (searchFor query summaries))
          , viewOldSummaries (searchFor query oldSummaries)
          ]



-- VIEW SUMMARY


viewSummary : Summary.Summary -> Html msg
viewSummary summary =
  let
    url =
      "/packages/" ++ summary.name ++ "/latest"
  in
    div [class "pkg-summary"]
      [ div []
          [ h1 [] [ a [ href url ] [ text summary.name ] ]
          , helpfulLinks summary
          ]
      , p [class "pkg-summary-desc"] [ text summary.summary ]
      ]


helpfulLinks : Summary.Summary -> Html msg
helpfulLinks summary =
  let
    allInterestingVersions =
      Vsn.filterInteresting summary.versions

    len =
      List.length allInterestingVersions

    interestingVersions =
      if len > 3 then
          List.drop (len - 3) allInterestingVersions

      else
          allInterestingVersions

    starter =
      case interestingVersions of
        (1,0,0) :: _ ->
          []

        _ ->
          [ text "…" ]
  in
    span [ class "pkg-summary-hints" ] <| List.intersperse (text " ") <|
      starter
      ++ List.intersperse (text "…") (List.map (versionLink summary.name) interestingVersions)
      ++  [ text "—"
          , a [ href ("/packages/" ++ summary.name) ] [ text "Overview" ]
          ]


versionLink : String -> Vsn.Version -> Html msg
versionLink packageName vsn =
  let
    vsnString =
      Vsn.vsnToString vsn

    url =
      "/packages/" ++ packageName ++ "/" ++ vsnString
  in
    a [ href url ] [ text vsnString ]


-- VIEW OLD SUMMARIES


viewOldSummaries : List Summary.Summary -> Html msg
viewOldSummaries oldSummaries =
  div [ style [ "opacity" => "0.5" ] ] <|
    if List.isEmpty oldSummaries then
      []

    else
      oldMessage :: List.map viewSummary oldSummaries


oldMessage : Html msg
oldMessage =
  p [ style
        [ "color" => "#EA157A"
        , "text-align" => "center"
        , "padding" => "1em"
        , "margin" => "0"
        , "background-color" => "#eeeeee"
        ]
    ]
    [ text "The following packages have not been updated for 0.18 yet!"
    ]
