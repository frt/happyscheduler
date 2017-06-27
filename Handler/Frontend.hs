module Handler.Frontend where

import Import

getFrontendR :: [Text] -> Handler Html
getFrontendR _ = defaultLayout $ do
        setTitle "happy scheduler"
        $(widgetFile "frontend")
