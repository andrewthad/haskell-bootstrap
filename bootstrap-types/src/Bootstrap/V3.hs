{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wall #-}

module Bootstrap.V3
  ( Context(..)
  , Size(..)
  , Columns(..)
  , Flow(..)
  , Panel(..)
  , NavbarTheme(..)
  , NavbarPosition(..)
  , NavbarItem(..)
  , NavbarDropdownItem(..)
  , contextClass
  , sizeClass
  , columnsClass
  , emptyColumns
  , singletonColumns
  ) where

import Data.Text (Text)
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

data Columns = Columns
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

columnsClass :: Columns -> Text
columnsClass (Columns xs sm md lg) = Text.concat
  [ "col-xs-",Text.pack (show xs)
  , " col-sm-",Text.pack (show sm)
  , " col-md-",Text.pack (show md)
  , " col-lg-",Text.pack (show lg)
  ]

emptyColumns :: Columns
emptyColumns = Columns 12 12 12 12

singletonColumns :: Size -> Int -> Columns
singletonColumns sz i = Columns
  (if sz >= ExtraSmall then i else 12)
  (if sz >= Small then i else 12)
  (if sz >= Medium then i else 12)
  (if sz >= Large then i else 12)

-- setColumns :: Size -> Int -> Columns -> Columns
-- setColumns sz i (Columns xs sm md lg) = Columns


