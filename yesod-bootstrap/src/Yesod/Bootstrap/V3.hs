{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Yesod.Bootstrap.V3 where

import Yesod.Core
import Yesod.Elements
import Text.Blaze (toValue)
import Data.Text (Text)
import Data.Monoid
import Control.Monad
import Data.Foldable (Foldable(fold))
import Data.Functor.Identity (Identity(..))
import Text.Julius (rawJS)
import Data.Foldable (for_)
import Control.Monad.Trans.Writer.Strict (tell,execWriterT)
import qualified Data.List as List
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import qualified Bootstrap.V3 as B
import qualified Data.Text as Text

panel :: B.Panel (WidgetT site IO ()) -> WidgetT site IO ()
panel (B.Panel title content ctx) = do
  div_ [class_ $ toValue $ "panel panel-" <> B.contextClass ctx] $ do
    div_ [class_  "panel-heading"] $ do
      h4_ [class_ "panel-title"] title
    div_ [class_ "panel-body"] $ do
      content

panelAccordion :: [B.Panel (WidgetT site IO ())] -> WidgetT site IO ()
panelAccordion tcs = do
  groupId <- newIdent
  div_ [class_ "panel-group",id_ (toValue $ groupId),H.customAttribute "role" "tablist"] $ do
    forM_ (zip [1..] tcs) $ \(i,B.Panel title content ctx) -> do
      headingId <- newIdent
      panelId <- newIdent
      div_ [class_ (toValue $ "panel panel-" <> B.contextClass ctx)] $ do
        div_ [class_  "panel-heading",H.customAttribute "role" "tab",id_ (toValue headingId)] $ do
          h4_ [class_ "panel-title"] $ do
            a_ [href_ (toValue $ "#" <> panelId),H.customAttribute "role" "button",H.dataAttribute "toggle" "collapse",H.dataAttribute "parent" (toValue ("#" <> groupId))] $ do
              title
        div_ [id_ (toValue panelId),class_ (toValue (Text.append "panel-collapse collapse" (if i == 1 then " in" else ""))),H.customAttribute "role" "tabpanel",H.customAttribute "aria-labelledby" (toValue headingId)] $ do
          div_ [class_ "panel-body"] $ do
            content

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

checkbox :: WidgetT site IO () -> WidgetT site IO ()
checkbox = div_ [class_ "checkbox"]

glyphicon :: Monad m => Text -> WidgetT site m ()
glyphicon s = span_ [class_ $ toValue $ "glyphicon glyphicon-" <> s] mempty

glyphiconFeedback :: Text -> WidgetT site IO ()
glyphiconFeedback s = span_ [class_ (toValue (Text.concat ["glyphicon glyphicon-", s, " form-control-feedback"]))] mempty

label :: B.Context -> WidgetT site IO () -> WidgetT site IO ()
label ctx = span_ [class_ $ toValue $ "label label-" <> B.contextClass ctx]

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

formGroup :: WidgetT site IO () -> WidgetT site IO ()
formGroup = div_ [class_ "form-group"]

formGroupFeedback :: B.Context -> WidgetT site IO () -> WidgetT site IO ()
formGroupFeedback ctx = div_ [class_ (toValue (Text.concat ["form-group has-", B.contextClass ctx, " has-feedback"]))]

inputGroup :: WidgetT site IO () -> WidgetT site IO ()
inputGroup = div_ [class_ "input-group"]

inputGroupAddon :: WidgetT site IO () -> WidgetT site IO ()
inputGroupAddon = span_ [class_ "input-group-addon"]

controlLabel :: WidgetT site IO () -> WidgetT site IO ()
controlLabel = label_ [class_ "control-label"]

helpBlock :: WidgetT site IO () -> WidgetT site IO ()
helpBlock = div_ [class_ "help-block"]

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

popoverClickable ::
     WidgetT site IO () -- ^ Title
  -> WidgetT site IO () -- ^ Popup
  -> WidgetT site IO () -- ^ Inner Content
  -> WidgetT site IO ()
popoverClickable title popup inner = do
  containerId <- newIdent
  innerId <- newIdent
  popupWrapId <- newIdent
  titleWrapId <- newIdent
  span_ [id_ $ toValue containerId] $ do
    a_ [href_ "javascript://", id_ $ toValue innerId] inner
    div_ [id_ $ toValue popupWrapId, style_' "display:none;"] $ do
      popup
    div_ [id_ $ toValue titleWrapId, style_' "display:none;"] $ do
      title
  toWidget [julius|
$().ready(function(){
  $('##{rawJS innerId}').popover(
    { html: true
    , trigger: 'manual'
    , content: function() { return $('##{rawJS popupWrapId}').html(); }
    , title: function() { return $('##{rawJS titleWrapId}').html(); }
    }
  );
  var hidePopover#{rawJS innerId} = function () {
    $('##{rawJS innerId}').popover('hide');
    $(document).off("click keypress", hidePopover#{rawJS innerId} );
  };
  $('##{rawJS innerId}').focusin(function() {
      $('##{rawJS innerId}').popover('show');
    });
  $('##{rawJS innerId}').on("shown.bs.popover", function() {
      $('##{rawJS containerId}').find(".popover").on("click keypress", function(e) {
          e.stopPropagation();
        });
      $(document).on("click keypress", hidePopover#{rawJS innerId});
    });
});
|]

carousel :: B.CarouselIndicators -> B.CarouselControls -> [B.CarouselItem (WidgetT site IO ())] -> WidgetT site IO ()
carousel indicators controls items = if length items == 0 then mempty else do
  carouselId <- newIdent
  div_ [class_ "carousel slide",H.dataAttribute "ride" "carousel",id_ (toValue carouselId)] $ do
    when (indicators == B.CarouselIndicatorsOn) $ do
      ol_ [class_ "carousel-indicators"] $ do
        forM_ (zip [0,1..] itemsActive) $ \(i,(active,_)) -> do
          li_ [ H.dataAttribute "target" (toValue ("#" <> carouselId))
              , H.dataAttribute "slide-to" (toValue (Text.pack (show i)))
              , class_ $ if active then "active" else ""
              ] mempty
    div_ [class_ "carousel-inner", H.customAttribute "role" "listbox"] $ do
      forM_ itemsActive $ \(active,item) -> do
        div_ [class_ $ toValue $ "item " <> if active then "active" else Text.empty] $ do
          wrapWithLink (B.carouselItemLink item) mempty
          img_ [src_ (toValue $ B.carouselItemImage item)]
          for_ (B.carouselItemCaption item) $ \caption -> do
            div_ [class_ "carousel-caption"] caption
    when (controls == B.CarouselControlsOn) $ do
      a_ [ class_ "left carousel-control"
         , href_ (toValue $ "#" <> carouselId)
         , H.customAttribute "role" "button"
         , H.dataAttribute "slide" "prev"
         ] $ do
         glyphicon "chevron-left"
         span_ [class_ "sr-only"] "Previous"
      a_ [ class_ "right carousel-control"
         , href_ (toValue $ "#" <> carouselId)
         , H.customAttribute "role" "button"
         , H.dataAttribute "slide" "next"
         ] $ do
         glyphicon "chevron-right"
         span_ [class_ "sr-only"] "Next"
  where 
  itemsActive = zip (True : repeat False) items
  wrapWithLink :: Maybe Text -> WidgetT site IO () -> WidgetT site IO ()
  wrapWithLink mroute w = (\ww -> maybe w ww mroute) $ \route -> do
    render <- getUrlRender
    a_ [href_ (toValue route),style_' "position:absolute;left:0;right:0;width:100%;height:100%;"] w

badge :: WidgetT site IO () -> WidgetT site IO ()
badge = span_ [class_ "badge"]

listGroupLinked :: [(Route site,WidgetT site IO ())] -> WidgetT site IO ()
listGroupLinked items = do
  render <- getUrlRender
  div_ [class_ "list-group"] $ forM_ items $ \(route,name) -> do
    a_ [href_ (toValue (render route)),class_ "list-group-item"] name

togglableTabs :: B.ToggleStyle -> [B.ToggleTab (WidgetT site IO ())] -> WidgetT site IO ()
togglableTabs s tabs = do
  (nav,bodies) <- execWriterT $ forM_ (zip [1..] tabs) $ \(i,tab) -> case tab of
    B.ToggleSection title body -> do -- WriterT (Widget,Widget) over a WidgetT
      theId <- lift newIdent
      let tabAAttrs = [H.customAttribute "role" "tab",href_ $ toValue $ "#" <> theId,H.dataAttribute "toggle" "tab"]
          tabLiAttrs = (if isFirst then [class_ "active"] else []) ++ [H.customAttribute "role" "presentation"]
          paneClasses = [class_ $ toValue $ "tab-pane " <> if isFirst then "active" else Text.empty,H.customAttribute "role" "tabpanel",id_ (toValue theId)]
          isFirst = (i == (1 :: Int))
      tellFst $ li_ tabLiAttrs $ a_ tabAAttrs $ toWidget title
      tellSnd $ div_ paneClasses body
    _ -> error "togglableTabs: figure this out"
  div_ [] $ do
    let styleText = case s of
          B.ToggleStyleTab -> "nav-tabs"
          B.ToggleStylePill -> "nav-pills"
    ul_ [class_ $ toValue $ "nav " <> (styleText :: Text),H.customAttribute "role" "tablist"] nav
    div_ [class_ "tab-content"] bodies
  where
  tellFst a = tell (a,mempty)
  tellSnd b = tell (mempty,b)
