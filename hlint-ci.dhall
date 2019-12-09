let haskellCi = https://raw.githubusercontent.com/vmchale/github-actions-dhall/master/haskell-ci.dhall sha256:864399147f8a2161eadd867f6dc22da794a25deae9a375fc3c98715960b4c07e

in  haskellCi.hlintAction [ "src", "test", "bench" ] : haskellCi.CI.Type
