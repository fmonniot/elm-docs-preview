module Page.Context exposing (..)

import Docs.Package as Docs
import Http
import Json.Decode as Json
import Json.Decode.Extra exposing ((|:))
import Regex
import Utils.Path exposing ((</>))


type alias GithubRepo =
    { repository : String
    , user : String
    , project : String
    }


type alias ElmPackage =
    { version : String
    , summary : String
    , license : String
    , sourceDirectories : List String
    , exposedModules : List String
    , nativeModules : Bool
    , dependencies : List ( String, String )
    , elmVersion : String
    , github : GithubRepo
    }



-- Request builder


getReadme : GithubRepo -> Http.Request String
getReadme context =
    Http.getString <| "https://raw.githubusercontent.com" </> context.user </> context.project </> "master/README.md"


getDocs : Http.Request Docs.Package
getDocs =
    Http.get "/documentation.json" Docs.decodePackage



-- Paths builder


pathToModule : String -> String
pathToModule m =
    "#" </> "module" </> m


pathTo : String -> String
pathTo file =
    "#" </> "module" </> file



-- Elm package Http and JSON


decodeElmPackage : Json.Decoder ElmPackage
decodeElmPackage =
    Json.succeed ElmPackage
        |: Json.field "version" Json.string
        |: Json.field "summary" Json.string
        |: Json.field "license" Json.string
        |: Json.field "source-directories" (Json.list Json.string)
        |: Json.field "exposed-modules" (Json.list Json.string)
        |: Json.field "native-modules" Json.bool
        |: Json.field "dependencies" (Json.keyValuePairs Json.string)
        |: Json.field "elm-version" Json.string
        |: decodeGithubRepository


decodeGithubRepository : Json.Decoder GithubRepo
decodeGithubRepository =
    let
        reg =
            Regex.regex "https:\\/\\/github\\.com\\/([\\w-_]+)\\/([\\w-_]+)\\.git"

        findIn str =
            Regex.find Regex.All reg str |> List.concatMap .submatches

        decode { repository } =
            case findIn repository of
                [ Just user, Just project ] ->
                    Json.succeed (GithubRepo repository user project)

                _ ->
                    Json.fail <| "The repository `" ++ repository ++ "` is not a valid GitHub repo"
    in
    Json.map (\a -> { repository = a })
        (Json.field "repository" Json.string)
        |> Json.andThen decode
