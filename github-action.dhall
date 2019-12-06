let haskellCi = https://raw.githubusercontent.com/vmchale/github-actions-dhall/master/haskell-ci.dhall

in  haskellCi.defaultWithSteps
                [ haskellCi.checkout
                , haskellCi.haskellEnv haskellCi.latestEnv
                , haskellCi.BuildStep.Name
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
