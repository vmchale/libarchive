let haskellCi =
      https://raw.githubusercontent.com/vmchale/github-actions-dhall/master/haskell-ci.dhall sha256:e48dbc8852b9a41fe15486b9963dd30940675219054939c7bb7a296030814b18

in  haskellCi.hlintAction [ "src", "test", "bench" ] : haskellCi.CI.Type
