{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Hydrant.Raw (
  -- * Elements
    textNode
  , textNodeUnescaped
  , parentNode
  , voidNode
  , tagOpen
  , tagClose
  -- * Escaping
  , escapeEntities
  ) where


import           Data.Char (Char)
import           Data.Foldable (Foldable (..))
import           Data.Functor (Functor(..))
import qualified Data.List as L
import           Data.Monoid (Monoid(..))
import           Data.String (IsString(..))
import           Data.Tuple (uncurry)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as TLB


textNode :: Text -> Builder
textNode =
  escapeEntities

textNodeUnescaped :: Text -> Builder
textNodeUnescaped =
  TLB.fromText

parentNode :: Text -> [(Text, Text)] -> Builder -> Builder
parentNode tag attrs b =
  tagOpen tag attrs <> b <> tagClose tag

voidNode :: Text -> [(Text, Text)] -> Builder
voidNode tag attrs =
     "<"
  <> fold (L.intersperse " " (TLB.fromText tag : fmap (uncurry attr) attrs))
  <> "/>"

tagOpen :: Text -> [(Text, Text)] -> Builder
tagOpen tag attrs =
     "<"
  <> fold (L.intersperse " " (TLB.fromText tag : fmap (uncurry attr) attrs))
  <> ">"

tagClose :: Text -> Builder
tagClose t =
  TLB.fromText ("</" <> t <> ">")

attr :: Text -> Text -> Builder
attr key val =
  TLB.fromText key <> TLB.fromText "=\"" <> escapeEntities val <> TLB.fromText "\""

-- -----------------------------------------------------------------------------
-- Escaping

escapeEntities :: (Monoid s, IsString s) => Text -> s
escapeEntities =
  T.foldr escapeCons mempty
{-# SPECIALIZE INLINE escapeEntities :: Text -> Text #-}
{-# SPECIALIZE INLINE escapeEntities :: Text -> Builder #-}

escapeCons :: (Monoid s, IsString s) => Char -> s -> s
escapeCons c =
  (<>) (escapeChar c)
{-# SPECIALIZE INLINE escapeCons :: Char -> Text -> Text #-}
{-# SPECIALIZE INLINE escapeCons :: Char -> Builder -> Builder #-}

escapeChar :: IsString s => Char -> s
escapeChar c =
  case c of
    '<'  -> "&lt;"
    '>'  -> "&gt;"
    '&'  -> "&amp;"
    '"'  -> "&quot;"
    '\'' -> "&#39;"
    x    -> fromString [x]
{-# SPECIALIZE INLINE escapeChar :: Char -> Text #-}
{-# SPECIALIZE INLINE escapeChar :: Char -> Builder #-}

-- -----------------------------------------------------------------------------

(<>) :: Monoid e => e -> e -> e
(<>) =
  mappend
{-# INLINE (<>) #-}
infixr 6 <>
