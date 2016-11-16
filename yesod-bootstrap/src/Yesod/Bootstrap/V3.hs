{-# LANGUAGE OverloadedStrings #-}

module Yesod.Bootstrap.V3 where

import Yesod.Core
import Yesod.Elements
import Text.Blaze (toValue)
import Data.Text (Text)
import Data.Monoid
import Control.Monad
import Data.Foldable (Foldable(fold))
import Data.Functor.Identity (Identity(..))
import qualified Data.List as List
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import qualified Bootstrap.V3 as B
import qualified Data.Text as Text

container :: Monad m => WidgetT site m a -> WidgetT site m a
container = div_ [class_ "container"]

row :: Monad m => WidgetT site m a -> WidgetT site m a
row = div_ [class_ "row"]

column :: Monad m => B.Column -> WidgetT site m a -> WidgetT site m a
column c = div_ [class_ (toValue (B.columnClass c))]

button :: Monad m => B.Context -> B.Size -> WidgetT site m a -> WidgetT site m a
button ctx size = button_
  [ class_ $ toValue $ B.buttonClass ctx size ]

formButtonPost :: B.Context -> B.Size -> Route site -> WidgetT site IO a -> WidgetT site IO a
formButtonPost = formButton "POST"

formButtonGet :: B.Context -> B.Size -> Route site -> WidgetT site IO a -> WidgetT site IO a
formButtonGet = formButton "GET"

formButton :: H.AttributeValue -> B.Context -> B.Size -> Route site -> WidgetT site IO a -> WidgetT site IO a
formButton method ctx size route inner = do
  urlRender <- getUrlRender
  form_ [method_ method, action_ (toValue (urlRender route))] $ do
    button ctx size inner

glyphicon :: Monad m => Text -> WidgetT site m ()
glyphicon s = span_ [class_ $ toValue $ "glyphicon glyphicon-" <> s] mempty

alert :: B.Context -> WidgetT site IO () -> WidgetT site IO ()
alert ctx = div_ [class_ $ toValue $ "alert alert-" <> B.contextClass ctx]

alertHtml :: B.Context -> Html -> Html
alertHtml ctx inner =
  H.div H.! HA.class_ (toValue $ "alert alert-" <> B.contextClass ctx) $ inner

anchor :: Foldable t => t H.Attribute -> Route site -> WidgetT site IO a -> WidgetT site IO a
anchor attrs route inner = do
  urlRender <- getUrlRender
  a_ (Identity $ (href_ $ toValue $ urlRender route) <> fold attrs) inner

anchorButton :: Foldable t => B.Context -> B.Size -> t H.Attribute -> Route site -> WidgetT site IO () -> WidgetT site IO ()
anchorButton ctx size attrs = anchor
  (Identity $ (class_ $ toValue $ B.buttonClass ctx size) <> fold attrs)

navbar ::
     B.NavbarTheme
  -> B.NavbarPosition
  -> Route site
  -> WidgetT site IO ()
  -> [B.NavbarItem (Route site) (WidgetT site IO ())]
  -> [B.NavbarItem (Route site) (WidgetT site IO ())]
  -> WidgetT site IO ()
navbar theme pos headerRoute headerContent items rightItems = do
  navbarId <- newIdent
  render <- getUrlRender
  nav_ [class_ $ toValue $ Text.concat
         [ "navbar "
         , B.navbarThemeClass theme
         , " "
         , B.navbarPositionClass pos
         ]
       ] $ do
    div_ [class_ containerClass] $ do
      div_ [class_ "navbar-header"] $ do
        button_ [ class_ "navbar-toggle collapsed"
                , type_ "button"
                , H.dataAttribute "toggle" "collapse"
                , H.dataAttribute "target" (toValue $ "#" <> navbarId)
                , H.customAttribute "aria-expanded" "false"
                , H.customAttribute "aria-controls" (toValue navbarId)
                ] $ do
          span_ [class_ "sr-only"] "Toggle Navigation"
          replicateM_ 3 $ span_ [class_ "icon-bar"] mempty
        a_ [href_ $ toValue $ render headerRoute, class_ "navbar-brand"] headerContent
      div_ [class_ "navbar-collapse collapse", id_ $ toValue navbarId] $ do
        ul_ [class_ "nav navbar-nav"] $ mapM_ navbarItem items
        ul_ [class_ "nav navbar-nav navbar-right"] $ mapM_ navbarItem rightItems
  where
  containerClass = case pos of
    B.NavbarPositionStandard -> "container-fluid"
    B.NavbarPositionStatic -> "container"
    B.NavbarPositionFixed -> "container"

navbarItem :: B.NavbarItem (Route site) (WidgetT site IO ()) -> WidgetT site IO ()
navbarItem item = do
  render <- getUrlRender
  li_ [] $ case item of
    B.NavbarItemLink route name -> anchor [] route name
    B.NavbarItemDropdown name children -> do
      a_ [ class_ "dropdown-toggle"
         , href_ "#"
         , H.customAttribute "role" "button"
         , H.dataAttribute "toggle" "dropdown"
         ] name
      ul_ [class_ "dropdown-menu"] $ mapM_ navbarDropdownItem children

navbarDropdownItem :: B.NavbarDropdownItem (Route site) (WidgetT site IO ()) -> WidgetT site IO ()
navbarDropdownItem item = do
  render <- getUrlRender
  case item of
    B.NavbarDropdownItemLink route name -> li_ [] $ anchor [] route name
    B.NavbarDropdownItemHeader name -> li_ [class_ "dropdown-header"] name
    B.NavbarDropdownItemSeparator -> li_ [class_ "separator", H.customAttribute "role" "divider"] mempty

breadcrumbsList :: [(Route site,WidgetT site IO ())] -> WidgetT site IO ()
breadcrumbsList allCrumbs = case List.reverse allCrumbs of
  (_,lastCrumbWidget):crumbs -> ol_ [class_ "breadcrumb"] $ do
    forM_ (List.reverse crumbs) $ \(route,name) -> li_ [] $ anchor [] route name
    li_ [class_ "active"] lastCrumbWidget
  [] -> mempty

