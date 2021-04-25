{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "heyduggee"
, dependencies = [ "console", "effect", "express", "psci-support" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
