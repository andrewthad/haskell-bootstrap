{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wall #-}

module Bootstrap.V3
  ( Context(..)
  , Size(..)
  , Column(..)
  , Flow(..)
  , Panel(..)
  , NavbarTheme(..)
  , NavbarPosition(..)
  , NavbarItem(..)
  , NavbarDropdownItem(..)
  , CarouselItem(..)
  , CarouselIndicators(..)
  , CarouselControls(..)
  , ToggleTab(..)
  , ToggleStyle(..)
  , contextClass
  , sizeClass
  , columnClass
  , buttonClass
  , navbarThemeClass
  , navbarPositionClass
  , emptyColumn
  , singletonColumn
  ) where

import Data.Text (Text)
import Data.Monoid
import qualified Data.Text as Text

data NavbarTheme
  = NavbarThemeDefault
  | NavbarThemeInverse
  | NavbarThemeOther Text

data NavbarPosition
  = NavbarPositionStandard
  | NavbarPositionStatic
  | NavbarPositionFixed

data NavbarItem r c
  = NavbarItemLink r c
  | NavbarItemDropdown c [NavbarDropdownItem r c]

data NavbarDropdownItem r c
  = NavbarDropdownItemLink r c
  | NavbarDropdownItemHeader c
  | NavbarDropdownItemSeparator

data Context
  = Success
  | Info
  | Warning
  | Danger
  | Default
  | Primary
  | Link
  | Error

data Size = ExtraSmall | Small | Medium | Large
  deriving (Eq,Ord)

data Column = Column
  { columnsExtraSmall :: !Int
  , columnsSmall :: !Int
  , columnsMedium :: !Int
  , columnsLarge :: !Int
  }

data Flow = Block | Inline

data Panel c = Panel
  { panelTitle :: !c
  , panelBody :: !c
  , panelContext :: !Context
  }

data CarouselItem c = CarouselItem
  { carouselItemImage :: Text
    -- ^ The image should be a hyperlink
  , carouselItemLink :: Maybe Text
    -- ^ This should be a hyperlink to the resource
  , carouselItemCaption :: Maybe c
  }

data CarouselIndicators = CarouselIndicatorsOn | CarouselIndicatorsOff
  deriving (Eq)
data CarouselControls = CarouselControlsOn | CarouselControlsOff
  deriving (Eq)

data ToggleTab c
  = ToggleSection Text c
  | ToggleDropdown Text [(Text,c)]

data ToggleStyle
  = ToggleStyleTab
  | ToggleStylePill

contextClass :: Context -> Text
contextClass x = case x of
  Success -> "success"
  Info -> "info"
  Warning -> "warning"
  Default -> "default"
  Primary -> "primary"
  Link -> "link"
  Error -> "error"
  Danger -> "danger"

sizeClass :: Size -> Text
sizeClass x = case x of
  ExtraSmall -> "xs"
  Small -> "sm"
  Medium -> "md"
  Large -> "lg"

columnClass :: Column -> Text
columnClass (Column xs sm md lg) = Text.concat
  [ "col-xs-",Text.pack (show xs)
  , " col-sm-",Text.pack (show sm)
  , " col-md-",Text.pack (show md)
  , " col-lg-",Text.pack (show lg)
  ]

emptyColumn :: Column
emptyColumn = Column 12 12 12 12

singletonColumn :: Size -> Int -> Column
singletonColumn sz i = Column
  (if sz >= ExtraSmall then i else 12)
  (if sz >= Small then i else 12)
  (if sz >= Medium then i else 12)
  (if sz >= Large then i else 12)

buttonClass :: Context -> Size -> Text
buttonClass ctx size = Text.concat
  [ "btn btn-"
  , contextClass ctx
  , " btn-"
  , sizeClass size
  ]

navbarThemeClass :: NavbarTheme -> Text
navbarThemeClass x = case x of
  NavbarThemeDefault -> "navbar-default"
  NavbarThemeInverse -> "navbar-inverse"
  NavbarThemeOther t -> "navbar-" <> t

navbarPositionClass :: NavbarPosition -> Text
navbarPositionClass x = case x of
  NavbarPositionStandard -> ""
  NavbarPositionStatic -> "navbar-static-top"
  NavbarPositionFixed -> "navbar-fixed-top"

-- setColumns :: Size -> Int -> Column -> Column
-- setColumns sz i (Column xs sm md lg) = Column


