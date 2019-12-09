let haskellCi =
      https://raw.githubusercontent.com/vmchale/github-actions-dhall/master/haskell-ci.dhall sha256:864399147f8a2161eadd867f6dc22da794a25deae9a375fc3c98715960b4c07e

let installLibarchive =
      haskellCi.BuildStep.Name
        { name = "Install libarchive"
        , run =
            ''
            wget https://www.libarchive.org/downloads/libarchive-3.4.0.tar.gz
            tar xvf libarchive-3.4.0.tar.gz
            cd libarchive-3.4.0
            ./configure
            make -j
            sudo make install
            cd -
            ''
        }

in    haskellCi.generalCi
        [ haskellCi.checkout
        , haskellCi.haskellEnv haskellCi.matrixEnv
        , installLibarchive
        , haskellCi.cabalDeps
        , haskellCi.cabalBuild
        , haskellCi.BuildStep.Name { name = "Get test data", run = "make -j" }
        , haskellCi.cabalTest
        , haskellCi.cabalDoc
        ]
        ( Some
            { ghc =
                [ haskellCi.GHC.GHC844
                , haskellCi.GHC.GHC865
                , haskellCi.GHC.GHC881
                ]
            , cabal = [ haskellCi.Cabal.Cabal30 ]
            , operating-system = [ haskellCi.OS.Ubuntu1804 ]
            }
        )
    : haskellCi.CI.Type
