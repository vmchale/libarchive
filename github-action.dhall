let haskellCi = https://raw.githubusercontent.com/vmchale/github-actions-dhall/master/haskell-ci.dhall

in  haskellCi.defaultWithSteps
                [ haskellCi.checkout
                , haskellCi.haskellEnv haskellCi.latestEnv
                , haskellCi.cabalDeps
                , haskellCi.cabalBuild
                , haskellCi.BuildStep.Name
                    { name = "Get test data"
                    , run =
                        ''
                        make -j
                        ''
                    }
                , haskellCi.cabalTest
                , haskellCi.cabalDoc
                ]
