module Handler.Frontend where

import Import

-- will redirect to login page if not authenticated
getFrontendR :: [Text] -> Handler Html
getFrontendR _ = do
    _ <- requireAuth
    defaultLayout $ do
        setTitle "happy scheduler"
        $(widgetFile "frontend")
