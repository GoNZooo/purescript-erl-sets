{ name = "erl-sets"
, dependencies =
  [ "assert", "console", "effect", "erl-lists", "erl-test-eunit", "prelude" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, backend = "purerl"
}
