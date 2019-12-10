let haskellCi =
      https://raw.githubusercontent.com/vmchale/github-actions-dhall/master/haskell-ci.dhall sha256:979469f6068f4bfa5e205f6a6b6faa02ae2bf9159425949075af242ce96e5df4

in  haskellCi.hlintAction [ "src", "test", "bench" ] : haskellCi.CI.Type
