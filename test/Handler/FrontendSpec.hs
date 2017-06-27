module Handler.FrontendSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $
    describe "getFrontendR" $ do
        it "loads the frontend index and checks it looks right" $ do
          userEntity <- createUser "baz"
          authenticateAs userEntity
          get $ FrontendR ["foobar"]
          statusIs 200
          htmlAllContain "title" "happy scheduler"
          htmlAnyContain "app-root" "Loading..."

