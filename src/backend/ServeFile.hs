{-# LANGUAGE OverloadedStrings #-}
module ServeFile (elm, pkgDocs, pkgOverview, pkgPreview) where

import Control.Monad.Trans (liftIO)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Time.Clock.POSIX (getPOSIXTime)
import Snap.Core (Snap, writeBuilder)
import System.IO.Unsafe (unsafePerformIO)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Utf8 as Blaze

import qualified Elm.Compiler.Module as Module
import qualified Elm.Package as Pkg
import qualified PackageSummary as PkgSummary
import qualified Path



-- TYPICAL PAGES / NO PORTS


elm :: String -> [String] -> Snap ()
elm title elmModuleName =
  makeHtml title elmModuleName Nothing (return Nothing)



-- DOCUMENTATION FOR A PARTICULAR VERSION


pkgDocs :: Pkg.Name -> Pkg.Version -> Maybe Module.Raw -> Snap ()
pkgDocs pkg@(Pkg.Name user project) version maybeName =
  let
    versionString =
      Pkg.versionToString version

    maybeStringName =
      fmap Module.nameToString maybeName

    title =
      maybe "" (++" - ") maybeStringName ++ project ++ " " ++ versionString

    maybeLink =
      Just (canonicalLink pkg maybeName)
  in
    makeHtml title ["Page","Package"] maybeLink $
      do  allVersions <- getAllVersions pkg
          return $ Just $ makeContext $
            [ ("user", show user)
            , ("project", show project)
            , ("version", show versionString)
            , ("allVersions", show allVersions)
            , ("moduleName", maybe "null" show maybeStringName)
            ]


canonicalLink :: Pkg.Name -> Maybe Module.Raw -> H.Html
canonicalLink pkg maybeName =
  let
    (Pkg.Name user project) =
      Map.findWithDefault pkg pkg renames

    ending =
      maybe "" (\name -> "/" ++ Module.nameToString name) maybeName

    url =
      "/packages/" ++ user ++ "/" ++ project ++ "/latest" ++ ending
  in
    link ! rel "canonical" ! href (toValue url)


renames :: Map.Map Pkg.Name Pkg.Name
renames =
  Map.fromList
    [ Pkg.Name "evancz" "elm-http" ==> Pkg.Name "elm-lang" "http"
    , Pkg.Name "evancz" "elm-html" ==> Pkg.Name "elm-lang" "html"
    , Pkg.Name "evancz" "elm-svg" ==> Pkg.Name "elm-lang" "svg"
    , Pkg.Name "evancz" "virtual-dom" ==> Pkg.Name "elm-lang" "virtual-dom"
    , Pkg.Name "evancz" "start-app" ==> Pkg.Name "elm-lang" "html"
    , Pkg.Name "evancz" "elm-effects" ==> Pkg.Name "elm-lang" "core"
    , Pkg.Name "elm-community" "elm-list-extra" ==> Pkg.Name "elm-community" "list-extra"
    , Pkg.Name "elm-community" "elm-linear-algebra" ==> Pkg.Name "elm-community" "linear-algebra"
    , Pkg.Name "elm-community" "elm-lazy-list" ==> Pkg.Name "elm-community" "lazy-list"
    , Pkg.Name "elm-community" "elm-json-extra" ==> Pkg.Name "elm-community" "json-extra"
    ]


(==>) :: a -> b -> (a, b)
(==>) =
  (,)



-- SHOW ALL THE DIFFERENT VERSIONS OF A PACKAGE


pkgOverview :: Pkg.Name -> Snap ()
pkgOverview pkg@(Pkg.Name user project) =
  makeHtml (user ++ "/" ++ project) ["Page","PackageOverview"] Nothing $
    do  allVersions <- getAllVersions pkg
        return $ Just $ makeContext $
          [ ("user", show user)
          , ("project", show project)
          , ("versions", show allVersions)
          ]


makeContext :: [(String, String)] -> (String, String)
makeContext entries =
  let
    ports =
      "{"
      ++ List.intercalate "," (List.map (\(k,v) -> "\n    " ++ k ++ ": " ++ v) entries)
      ++ "\n}"
  in
    (ports, "")


getAllVersions :: Pkg.Name -> Snap [String]
getAllVersions pkg =
  do  maybeVersions <- liftIO (PkgSummary.readVersionsOf pkg)
      return $ maybe [] (List.map Pkg.versionToString) maybeVersions



-- PREVIEW DOCUMENTATION


pkgPreview :: Snap ()
pkgPreview =
  makeHtml "Preview your Docs" ["Page","PreviewDocumentation"] Nothing $
    return $ Just $ (,) "" $
      "function handleFileSelect(evt) {\n\
      \    var reader = new FileReader();\n\
      \    reader.readAsText(evt.target.files[0]);\n\
      \    reader.onload = function(event) {\n\
      \        page.ports.uploads.send(event.target.result);\n\
      \    };\n\
      \}\n\
      \\n\
      \setTimeout(function() {\n\
      \  document.getElementById('fileLoader').addEventListener('change', handleFileSelect, false);\n\
      \}, 0)\n"



-- SKELETON


makeHtml :: String -> [String] -> Maybe H.Html -> Snap (Maybe (String, String)) -> Snap ()
makeHtml title elmModule maybeLink makePorts =
  do  maybePorts <- makePorts
      writeBuilder $ Blaze.renderHtmlBuilder $ docTypeHtml $ do
        H.head $ do
          meta ! charset "UTF-8"
          favicon
          H.title (toHtml title)
          googleAnalytics
          Maybe.fromMaybe (return ()) maybeLink
          link ! rel "stylesheet" ! href (cacheBuster "/assets/highlight/styles/default.css")
          link ! rel "stylesheet" ! href (cacheBuster "/assets/style.css")
          script ! src (cacheBuster "/assets/highlight/highlight.pack.js") $ ""
          script ! src (cacheBuster ("/" ++ Path.artifact elmModule)) $ ""

        body $
          script $ preEscapedToMarkup $
            case maybePorts of
              Nothing ->
                "\nElm." ++ Module.nameToString elmModule ++ ".fullscreen()\n"

              Just (ports, postScript) ->
                "\nvar page = Elm."
                ++ Module.nameToString elmModule
                ++ ".fullscreen("
                ++ ports
                ++ ");\n\n"
                ++ postScript


googleAnalytics :: Html
googleAnalytics =
  script ! type_ "text/javascript" $
    "(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){\n\
    \(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),\n\
    \m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)\n\
    \})(window,document,'script','//www.google-analytics.com/analytics.js','ga');\n\
    \\n\
    \ga('create', 'UA-25827182-1', 'auto');\n\
    \ga('send', 'pageview');\n"


favicon :: H.Html
favicon =
  H.link
    ! A.rel "shortcut icon"
    ! A.size "16x16, 32x32, 48x48, 64x64, 128x128, 256x256"
    ! A.href "/assets/favicon.ico"


cacheBuster :: String -> AttributeValue
cacheBuster url =
  toValue (url ++ "?" ++ uniqueToken)


uniqueToken :: String
uniqueToken =
  unsafePerformIO (show <$> round <$> getPOSIXTime)
