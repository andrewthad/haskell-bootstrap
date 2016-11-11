module Yesod.Elements where

import Yesod.Core.Types (Body(..),GWData(..),WidgetT(..))
import Text.Blaze.Html (Html,Attribute)
import Data.Foldable
import qualified Text.Blaze.Html as H

insideElement :: Foldable t => (Html -> Html) -> t Attribute -> WidgetT site IO a -> WidgetT site IO a
insideElement el attrs (WidgetT f) = WidgetT $ \hdata -> do
  -- render <- getUrlRender
  (a,gwd) <- f hdata
  let Body bodyFunc = gwdBody gwd
      combinedAttrs = fold attrs
      newBodyFunc render =
        el H.! combinedAttrs $ (bodyFunc render)
  return (a,gwd { gwdBody = Body newBodyFunc })

