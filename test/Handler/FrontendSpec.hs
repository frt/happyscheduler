module Handler.FrontendSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $
    describe "getFrontendR" $ do
        it "loads the frontend index and checks it looks right" $ do
            getWithAuthenticatedUser (FrontendR []) "foo"
            htmlAllContain "title" "happy scheduler"
            htmlAnyContain "app-root" "Loading..."
        
        it "shows 'My Tasks' in the navbar" $ do
            getWithAuthenticatedUser (FrontendR []) "bar"
            htmlAnyContain "ul.nav.navbar-nav > li > a" "My Tasks"

