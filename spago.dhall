{ name = "erl-sets"
, dependencies =
  [ "arrays"
  , "assert"
  , "effect"
  , "erl-lists"
  , "erl-test-eunit"
  , "foldable-traversable"
  , "prelude"
  , "quickcheck"
  , "unfoldable"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, backend = "purerl"
}
