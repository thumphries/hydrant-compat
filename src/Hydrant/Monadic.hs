{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Hydrant.Monadic (
    Html (..)
  , runHtml
  , liftHtml
  , parentNode
  , voidNode
  , textNode
  , textNodeUnescaped
  ) where


import           Control.Monad.Trans.Writer (WriterT (..), Writer, runWriter, tell)
import           Control.Applicative
import           Control.Monad
import           Data.Function (($), (.))
import           Data.Text (Text)
import           Data.Tuple (snd)
import           Hydrant (Attribute, Tag (..))
import qualified Hydrant


newtype Html a = Html {
    unHtml :: Writer Hydrant.Html a
  } deriving (Functor, Applicative, Monad)

runHtml :: Html a -> Hydrant.Html
runHtml =
  snd . runWriter . unHtml
{-# INLINE runHtml #-}

liftHtml :: Hydrant.Html -> Html ()
liftHtml =
  Html . tell
{-# INLINE liftHtml #-}

parentNode :: Hydrant.Tag -> [Hydrant.Attribute] -> Html () -> Html ()
parentNode tag attrs body =
  liftHtml $
    Hydrant.parentNode
      tag
      attrs
      (runHtml body)
{-# INLINE parentNode #-}

voidNode :: Hydrant.Tag -> [Hydrant.Attribute] -> Html ()
voidNode tag attrs =
  liftHtml $
    Hydrant.voidNode
      tag
      attrs
{-# INLINE voidNode #-}

textNode :: Text -> Html ()
textNode t =
  liftHtml $
    Hydrant.textNode t
{-# INLINE textNode #-}

textNodeUnescaped :: Text -> Html ()
textNodeUnescaped t=
  liftHtml $
    Hydrant.textNodeUnescaped t
{-# INLINE textNodeUnescaped #-}
