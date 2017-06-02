module Route exposing (..)

import Page.Context as Ctx
import UrlParser as Url exposing ((</>), (<?>), s, string, top)


{-
   - Help is something to be defined
   - Readme is the README page
   - Module is the page for a given module
-}
-- TODO Rename to Route


type R
    = Help
    | Readme
    | Module String


parser : Url.Parser (R -> a) a
parser =
    Url.oneOf
        [ Url.map Readme top
        , Url.map Help (s "help")
        , Url.map Module (s "module" </> string)
        ]


(=>) f a =
    f (Just a)
infixr 0 =>
