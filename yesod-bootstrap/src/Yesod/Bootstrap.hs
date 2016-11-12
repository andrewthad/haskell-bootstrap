module Yesod.Bootstrap where

import qualified Bootstrap.V3 as B
import Yesod.Core
import Yesod.Core.Widget
import Yesod.Core.Handler
import Data.Text (Text)
import Data.List
import Data.Monoid
import Control.Monad
import Text.Blaze.Html (toHtml)
import qualified Data.Text as Text
import Control.Monad.Writer.Class
import Control.Monad.Writer.Strict
import qualified Data.List as List
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import Data.Function (on)
import Data.String (IsString(..))
import Data.Char (isDigit)
import Text.Julius (rawJS)

basicPanel :: Text -> WidgetT site IO () -> Panel site
basicPanel t c = Panel (tw t) c Default

anchor :: Route site -> WidgetT site IO () -> WidgetT site IO ()
anchor route inner = [whamlet|<a href="@{route}">^{inner}|]

anchorEmail :: Text -> WidgetT site IO () -> WidgetT site IO ()
anchorEmail email inner = [whamlet|<a href="mailto:#{email}">^{inner}|]

anchorPhone :: Text -> WidgetT site IO ()
anchorPhone phone = [whamlet|<a href="tel:#{cleanedPhone}">#{phone}|]
  where
  strippedPhone = Text.filter ((||) <$> isDigit <*> (== '+')) phone
  cleanedPhone = case Text.uncons strippedPhone of
    Nothing -> Text.empty
    Just (c,cs) -> if c == '+'
      then strippedPhone
      else Text.append "+1" strippedPhone

row :: WidgetT site IO () -> WidgetT site IO ()
row = div_ [("class","row")]

container :: WidgetT site IO () -> WidgetT site IO ()
container = div_ [("class","container")]

well :: Size -> WidgetT site IO () -> WidgetT site IO ()
well size = div_ [("class","well well-" <> colSizeShortName size)]

col :: [ColSize] -> WidgetT site IO () -> WidgetT site IO ()
col cs = div_ [("class", Text.intercalate " " (map mkAttr cs))]
  where mkAttr (ColSize s n) = Text.concat ["col-", colSizeShortName s, "-", Text.pack (show n)]

checkbox :: WidgetT site IO () -> WidgetT site IO ()
checkbox = div_ [("class","checkbox")]

alert :: Context -> WidgetT site IO () -> WidgetT site IO ()
alert ctx = div_ [("class","alert alert-" <> contextName ctx)]

alertHtml :: Context -> Html -> Html
alertHtml ctx inner = H.div H.! HA.class_ (fromString $ Text.unpack $ "alert alert-" <> contextName ctx) $ inner

alertHtmlDismiss :: Context -> Html -> Html
alertHtmlDismiss ctx inner = H.div H.! HA.class_ (fromString $ Text.unpack $ "alert alert-dismissable alert-" <> contextName ctx) $ do
  H.button H.! HA.class_ "close" H.! HA.type_ "button" H.! H.dataAttribute "dismiss" "alert" $ H.preEscapedToHtml ("&times;" :: Text)
  inner

caret :: WidgetT site IO ()
caret = span_ [("class","caret")] mempty

glyphicon :: Text -> WidgetT site IO ()
glyphicon s = span_ [("class","glyphicon glyphicon-" <> s)] mempty

glyphiconFeedback :: Text -> WidgetT site IO ()
glyphiconFeedback s = span_ [("class",Text.concat ["glyphicon glyphicon-", s, " form-control-feedback"])] mempty

formGroup :: WidgetT site IO () -> WidgetT site IO ()
formGroup = div_ [("class","form-group")]

formGroupFeedback :: Context -> WidgetT site IO () -> WidgetT site IO ()
formGroupFeedback ctx = div_ [("class",Text.concat ["form-group has-", contextName ctx, " has-feedback"])]

inputGroup :: WidgetT site IO () -> WidgetT site IO ()
inputGroup = div_ [("class","input-group")]

inputGroupAddon :: WidgetT site IO () -> WidgetT site IO ()
inputGroupAddon = span_ [("class","input-group-addon")]

controlLabel :: WidgetT site IO () -> WidgetT site IO ()
controlLabel = label_ [("class","control-label")]

helpBlock :: WidgetT site IO () -> WidgetT site IO ()
helpBlock = div_ [("class","help-block")]

button :: Context -> Size -> WidgetT site IO () -> WidgetT site IO ()
button ctx size inner = do
  button_ [("class","btn btn-" <> contextName ctx <> " btn-" <> colSizeShortName size)] inner

buttonRaised :: Context -> Size -> WidgetT site IO () -> WidgetT site IO ()
buttonRaised ctx size inner = do
  button_ [("class","btn btn-raised btn-" <> contextName ctx <> " btn-" <> colSizeShortName size)] inner

buttonRaisedBlock :: Context -> Size -> WidgetT site IO () -> WidgetT site IO ()
buttonRaisedBlock ctx size inner = do
  button_ [("class","btn btn-raised btn-block btn-" <> contextName ctx <> " btn-" <> colSizeShortName size)] inner

formButtonPost :: Context -> Size -> Route site -> WidgetT site IO () -> WidgetT site IO ()
formButtonPost ctx size route inner = do
  render <- getUrlRender
  form_ [("method","POST"),("action",render route)] $ do
    button ctx size inner

formButtonRaisedPost :: Context -> Size -> Route site -> WidgetT site IO () -> WidgetT site IO ()
formButtonRaisedPost ctx size route inner = do
  render <- getUrlRender
  form_ [("method","POST"),("action",render route)] $ do
    buttonRaised ctx size inner

formButtonRaisedPostBlock :: Context -> Size -> Route site -> WidgetT site IO () -> WidgetT site IO ()
formButtonRaisedPostBlock ctx size route inner = do
  render <- getUrlRender
  form_ [("method","POST"),("action",render route)] $ do
    buttonRaisedBlock ctx size inner

anchorButton :: Context -> Size -> Route site -> WidgetT site IO () -> WidgetT site IO ()
anchorButton ctx size route inner = do
  render <- getUrlRender
  a_ [("href",render route),("class","btn btn-" <> contextName ctx <> " btn-" <> colSizeShortName size)] inner

anchorButtonDropdown :: Context -> Size -> WidgetT site IO () -> [(Route site, WidgetT site IO ())] -> WidgetT site IO ()
anchorButtonDropdown ctx size inner xs = div_ [("class","btn-group")] $ do
  button_ [ ("type","button")
          , ("class","btn btn-" <> contextName ctx <> " btn-" <> colSizeShortName size)
          , ("data-toggle","dropdown")
          , ("aria-haspopup","true")
          , ("aria-expanded","false")
          ] inner
  ul_ [("class","dropdown-menu")] $ forM_ xs $ \(route,content) -> do
    li_ [] $ anchor route content

anchorButtonRaised :: Context -> Size -> Route site -> WidgetT site IO () -> WidgetT site IO ()
anchorButtonRaised ctx size route inner = do
  render <- getUrlRender
  a_ [("href",render route),("class","btn btn-raised btn-" <> contextName ctx <> " btn-" <> colSizeShortName size)] inner

anchorButtonBlock :: Context -> Size -> Route site -> WidgetT site IO () -> WidgetT site IO ()
anchorButtonBlock ctx size route inner = do
  render <- getUrlRender
  a_ [("href",render route),("class","btn btn-block btn-" <> contextName ctx <> " btn-" <> colSizeShortName size)] inner

anchorButtonRaisedBlock :: Context -> Size -> Route site -> WidgetT site IO () -> WidgetT site IO ()
anchorButtonRaisedBlock ctx size route inner = do
  render <- getUrlRender
  a_ [("href",render route),("class","btn btn-raised btn-block btn-" <> contextName ctx <> " btn-" <> colSizeShortName size)] inner

label :: Context -> WidgetT site IO () -> WidgetT site IO ()
label ctx = span_ [("class","label label-" <> contextName ctx)]

badge :: WidgetT site IO () -> WidgetT site IO ()
badge = span_ [("class","badge")]

panel :: Panel site -> WidgetT site IO ()
panel (Panel title content ctx) = do
  div_ [("class","panel panel-" <> contextName ctx)] $ do
    div_ [("class", "panel-heading")] $ do
      h4_ [("class","panel-title")] title
    div_ [("class","panel-body")] $ do
      content

panelAccordion :: [Panel site] -> WidgetT site IO ()
panelAccordion tcs = do
  groupId <- newIdent
  div_ [("class","panel-group"),("id",groupId),("role","tablist")] $ do
    forM_ (zip [1..] tcs) $ \(i,Panel title content ctx) -> do
      headingId <- newIdent
      panelId <- newIdent
      div_ [("class","panel panel-" <> contextName ctx)] $ do
        div_ [("class", "panel-heading"),("role","tab"),("id",headingId)] $ do
          h4_ [("class","panel-title")] $ do
            a_ [("href","#" <> panelId),("role","button"),("data-toggle","collapse"),("data-parent","#" <> groupId)] $ do
              title
        div_ [("id",panelId),("class","panel-collapse collapse" <> (if i == 1 then " in" else "")),("role","tabpanel"),("aria-labelledby",headingId)] $ do
          div_ [("class","panel-body")] $ do
            content

textSubmitGroupGetForm :: Route site -> Context -> Size -> Text -> Text -> Text -> WidgetT site IO () -> Bool -> WidgetT site IO ()
textSubmitGroupGetForm route ctx size name placeholder value buttonContent buttonIsLeft = do
  render <- getUrlRender
  form_ [("method","GET"),("action",render route)] $ do
    div_ [("class","form-group")] $ do
      div_ [("class","input-group input-group-" <> colSizeShortName size)]
        $ mconcat
        $ (if buttonIsLeft then reverse else id)
        [ input_ [("class","form-control"),("type","text"),("name",name),("placeholder",placeholder),("value",value)]
        , span_ [("class","input-group-btn")] $ do
            button_ [("class","btn btn-" <> contextName ctx)] buttonContent
        ]

navbar ::
     NavbarTheme
  -> NavbarPosition
  -> Route site
  -> WidgetT site IO ()
  -> [NavbarItem site]
  -> [NavbarItem site]
  -> WidgetT site IO ()
navbar theme pos headerRoute headerContent items rightItems = do
  navbarId <- newIdent
  render <- getUrlRender
  nav_ [("class","navbar " <> themeClass <> " " <> posClass)] $ do
    div_ [("class",containerClass)] $ do
      div_ [("class", "navbar-header")] $ do
        button_ [ ("class", "navbar-toggle collapsed"),("type","button")
                , ("data-toggle", "collapse"), ("aria-expanded", "false")
                , ("aria-controls", navbarId),("data-target", "#" <> navbarId)
                ] $ do
          span_ [("class","sr-only")] $ tw "Toggle Navigation"
          replicateM_ 3 $ span_ [("class","icon-bar")] mempty
        a_ [("href", render headerRoute),("class","navbar-brand")] headerContent
      div_ [("class","navbar-collapse collapse"), ("id", navbarId)] $ do
        ul_ [("class","nav navbar-nav")] $ mapM_ navbarItem items
        ul_ [("class","nav navbar-nav navbar-right")] $ mapM_ navbarItem rightItems
  where
  themeClass = case theme of
    NavbarDefault -> "navbar-default"
    NavbarInverse -> "navbar-inverse"
    NavbarOtherTheme t -> "navbar-" <> t
  posClass = case pos of
    NavbarStandard -> ""
    NavbarStaticTop -> "navbar-static-top"
    NavbarFixedTop -> "navbar-fixed-top"
  containerClass = case pos of
    NavbarStandard -> "container-fluid"
    NavbarStaticTop -> "container"
    NavbarFixedTop -> "container"

navbarItem :: NavbarItem site -> WidgetT site IO ()
navbarItem item = do
  render <- getUrlRender
  li_ [] $ case item of
    NavbarLink route name -> anchor route name
    NavbarDropdown name children -> do
      a_ [ ("class","dropdown-toggle"), ("href", "#")
         , ("role", "button"), ("data-toggle", "dropdown")
         ] name
      ul_ [("class","dropdown-menu")] $ mapM_ navbarDropdownItem children

navbarDropdownItem :: NavbarDropdownItem site -> WidgetT site IO ()
navbarDropdownItem item = do
  render <- getUrlRender
  case item of
    NavbarDropdownLink route name -> li_ [] $ anchor route name
    NavbarDropdownHeader name -> li_ [("class","dropdown-header")] name
    NavbarDropdownSeparator -> li_ [("class","separator"),("role","divider")] mempty

-- Stands for text widget
tw :: Text -> WidgetT site IO ()
tw = toWidget . toHtml

preEscapedWidget :: Text -> WidgetT site IO ()
preEscapedWidget = toWidget . H.preEscapedToHtml

data CarouselItem site = CarouselItem
  { ciImage   :: Route site
  , ciLink    :: Maybe (Route site)
  , ciCaption :: Maybe (WidgetT site IO ())
  }

data CarouselIndicators = CarouselIndicatorsOn | CarouselIndicatorsOff
  deriving (Eq)
data CarouselControls = CarouselControlsOn | CarouselControlsOff
  deriving (Eq)

-- Carousel Element
carousel :: CarouselIndicators -> CarouselControls -> [CarouselItem site] -> WidgetT site IO ()
carousel indicators controls items = if length items == 0 then mempty else do
  render <- getUrlRender
  carouselId <- newIdent
  div_ [("class","carousel slide"),("data-ride","carousel"),("id",carouselId)] $ do
    when (indicators == CarouselIndicatorsOn) $ do
      ol_ [("class","carousel-indicators")] $ do
        forM_ (zip [0,1..] itemsActive) $ \(i,(active,_)) -> do
          li_ [ ("data-target", "#" <> carouselId)
              , ("data-slide-to", Text.pack (show i))
              , ("class", if active then "active" else "")
              ] mempty
    div_ [("class","carousel-inner"), ("role","listbox")] $ do
      forM_ itemsActive $ \(active,item) -> do
        div_ [("class","item " <> if active then "active" else "")] $ do
          wrapWithLink (ciLink item) mempty
          img_ [("src",render (ciImage item))]
          for_ (ciCaption item) $ \caption -> do
            div_ [("class","carousel-caption")] caption
    when (controls == CarouselControlsOn) $ do
      a_ [ ("class","left carousel-control")
         , ("href","#" <> carouselId)
         , ("role","button")
         , ("data-slide","prev")
         ] $ do
         glyphicon "chevron-left"
         span_ [("class","sr-only")] "Previous"
      a_ [ ("class","right carousel-control")
         , ("href","#" <> carouselId)
         , ("role","button")
         , ("data-slide","next")
         ] $ do
         glyphicon "chevron-right"
         span_ [("class","sr-only")] "Next"
  where
  itemsActive = zip (True : repeat False) items
  wrapWithLink :: Maybe (Route site) -> WidgetT site IO () -> WidgetT site IO ()
  wrapWithLink mroute w = (\ww -> maybe w ww mroute) $ \route -> do
    render <- getUrlRender
    a_ [("href", render route),("style","position:absolute;left:0;right:0;width:100%;height:100%;")] w

-- Togglable tabs
data ToggleTab site = ToggleSection Text (WidgetT site IO ()) | ToggleDropdown Text [(Text,WidgetT site IO ())]
data ToggleStyle = ToggleStyleTab | ToggleStylePill

togglableTabs :: ToggleStyle -> [ToggleTab site] -> WidgetT site IO ()
togglableTabs s tabs = do
  (nav,bodies) <- execWriterT $ forM_ (zip [1..] tabs) $ \(i,tab) -> case tab of
    ToggleSection title body -> do -- WriterT (Widget,Widget) over a WidgetT
      theId <- lift newIdent
      let tabAAttrs = [("role","tab"),("href","#" <> theId),("data-toggle","tab")]
          tabLiAttrs = (if isFirst then addClass "active" else id) [("role","presentation")]
          paneClasses = (if isFirst then addClass "active" else id)
            [("class","tab-pane"),("role","tabpanel"),("id",theId)]
          isFirst = (i == (1 :: Int))
      tellFst $ li_ tabLiAttrs $ a_ tabAAttrs $ tw title
      tellSnd $ div_ paneClasses body
    _ -> error "figure this out"
  div_ [] $ do
    let styleText = case s of
          ToggleStyleTab -> "nav-tabs"
          ToggleStylePill -> "nav-pills"
    ul_ [("class","nav " <> styleText),("role","tablist")] nav
    div_ [("class","tab-content")] bodies
  where
  tellFst a = tell (a,mempty)
  tellSnd b = tell (mempty,b)
  addClass :: Text -> [(Text,Text)] -> [(Text,Text)]
  addClass klass attrs = case List.lookup "class" attrs of
    Nothing -> ("class",klass) : attrs
    Just c -> ("class",c <> " " <> klass) : List.deleteBy ((==) `on` fst) ("class","") attrs

radioButtons :: Context -> Text -> [(Text, WidgetT site IO ())] -> WidgetT site IO ()
radioButtons ctx name xs = do
  div_ [("class","btn-group"),("data-toggle","buttons")] $ do
    forM_ (zip trueThenFalse xs) $ \(isFirst, (theValue,w)) -> do
      label_ [("class", "btn btn-" <> contextName ctx <> if isFirst then " active" else "")] $ do
        input_ $ (if isFirst then [("checked","checked")] else [])
              ++ [("type","radio"),("name",name),("value",theValue),("autocomplete","off")]
        " "
        w

listGroupLinked :: [(Route site,WidgetT site IO ())] -> WidgetT site IO ()
listGroupLinked items = do
  render <- getUrlRender
  div_ [("class","list-group")] $ forM_ items $ \(route,name) -> do
    a_ [("href",render route),("class","list-group-item")] name

breadcrumbsList :: [(Route site,WidgetT site IO ())] -> WidgetT site IO ()
breadcrumbsList allCrumbs = case reverse allCrumbs of
  (_,lastCrumbWidget):crumbs -> ol_ [("class","breadcrumb")] $ do
    forM_ (reverse crumbs) $ \(route,name) -> li_ [] $ anchor route name
    li_ [("class","active")] lastCrumbWidget
  [] -> mempty

popover :: WidgetT site IO () -> WidgetT site IO () -> WidgetT site IO () -> WidgetT site IO ()
popover title popup inner = do
  innerId <- newIdent
  popupWrapId <- newIdent
  titleWrapId <- newIdent
  a_ [("href","javascript://"),("id",innerId)] inner
  div_ [("id",popupWrapId),("style","display:none;")] $ do
    popup
  div_ [("id",titleWrapId),("style","display:none;")] $ do
    title
  toWidget [julius|
$().ready(function(){
  $('##{rawJS innerId}').popover(
    { html: true
    , trigger: 'focus'
    , content: function() { return $('##{rawJS popupWrapId}').html(); }
    , title: function() { return $('##{rawJS titleWrapId}').html(); }
    }
  );
});
|]

popoverClickable :: WidgetT site IO () -> WidgetT site IO () -> WidgetT site IO () -> WidgetT site IO ()
popoverClickable title popup inner = do
  containerId <- newIdent
  innerId <- newIdent
  popupWrapId <- newIdent
  titleWrapId <- newIdent
  span_ [("id",containerId)] $ do
    a_ [("href","javascript://"),("id",innerId)] inner
    div_ [("id",popupWrapId),("style","display:none;")] $ do
      popup
    div_ [("id",titleWrapId),("style","display:none;")] $ do
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

trueThenFalse :: [Bool]
trueThenFalse = True : repeat False

