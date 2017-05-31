module Page.Context exposing (..)

import Docs.Package as Docs
import Http
import Utils.Path exposing ((</>))


type alias OverviewContext =
    { user : String
    , project : String
    , versions : List String
    }


type alias VersionContext =
    { user : String
    , project : String
    , version : String
    , allVersions : List String
    , moduleName : Maybe String
    }


getReadme : VersionContext -> Http.Request String
getReadme context =
    Http.getString <| "https://raw.githubusercontent.com" </> context.user </> context.project </> "master/README.md"


getDocs : VersionContext -> Http.Request Docs.Package
getDocs context =
    Http.get "/documentation.json" Docs.decodePackage


pathTo : VersionContext -> String -> String
pathTo { user, project, version } file =
    "/packages" </> user </> project </> version </> file
