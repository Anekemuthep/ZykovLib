{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Text.Blaze.Html5 as H hiding (main)
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Control.Monad.IO.Class (liftIO)
import Data.Monoid (mconcat)
import Data.Text.Lazy as LT
import Data.String (fromString)

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

main :: IO ()
main = scotty 3000 $ do
    get "/" $ do
        Web.Scotty.html . renderHtml $ do
            H.form ! A.action "/" ! A.method "post" $ do
                "Enter a number: "
                H.input ! A.type_ "text" ! A.name "number"
                H.input ! A.type_ "submit" ! A.value "Calculate factorial"
    post "/" $ do
        number <- Web.Scotty.param "number"
        let result = factorial number
        Web.Scotty.html . mconcat $ ["The result of factorial is: ", LT.pack (show result)]
