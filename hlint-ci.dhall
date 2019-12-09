let haskellCi = https://raw.githubusercontent.com/vmchale/github-actions-dhall/master/haskell-ci.dhall sha256:bb6b0eee75d9f5d9e62b7a0386efef5c1d0d6fb1415eab5a33500976cc70c886

in  haskellCi.hlintAction [ "src", "test", "bench" ] : haskellCi.CI.Type
