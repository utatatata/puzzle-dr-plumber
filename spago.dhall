{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "puzzle-dr-plumber"
, dependencies =
    [ "console"
    , "control"
    , "effect"
    , "foldable-traversable"
    , "free"
    , "lists"
    , "maybe"
    , "psci-support"
    , "random"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
