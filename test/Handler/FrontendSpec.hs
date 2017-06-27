module Handler.FrontendSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $
    describe "getFrontendR" $ do
        it "loads the frontend index and checks it looks right" $ do
          userEntity <- createUser "baz"
          authenticateAs userEntity
          get HomeR
          statusIs 200
          htmlAnyContain "app-root" "Loading..."
          htmlAllContain "title" "Welcome Happy Scheduler!"

