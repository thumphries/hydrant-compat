{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


import           Criterion.Main
import           Criterion.Types (Config(..))

import qualified Data.List as L
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL

import           P

import           Hydrant
import qualified Hydrant.Monadic as M

import qualified Lucid as Lucid
import qualified Lucid.Base as Lucid

import           System.IO

import qualified Text.Blaze as Blaze
import qualified Text.Blaze.Html5 as Blaze
import qualified Text.Blaze.Html.Renderer.Text as Blaze


-- -----------------------------------------------------------------------------
-- Hydrant

thing :: Html
thing =
  mconcat [
      doctype "HTML"
    , parentNode (Tag "div") [Attribute (AttributeKey "blink") (AttributeValue "160bpm")]
        (textNode "marquee marquee marquee netscape navigator")
    , voidNode (Tag "img") [Attribute (AttributeKey "src") (AttributeValue "google.com")]
    , parentNode (Tag "p") []
        (textNode "html is for you and me")
    ]

linear :: Int -> Html
linear n =
  mconcat (L.replicate n thing)

nested :: Int -> Html
nested 0 =
  thing
nested n =
  parentNode (Tag "div") [Attribute (AttributeKey "blink") (AttributeValue "210bpm")]
    (thing <> (nested (n-1)))

escape :: Text -> Int -> Text
escape t n =
  escapeEntities (T.replicate n t)

-- -----------------------------------------------------------------------------
-- Hydrant.Monadic

mthing :: M.Html ()
mthing = do
  M.liftHtml (doctype "HTML")
  M.parentNode (Tag "div") [Attribute (AttributeKey "blink") (AttributeValue "160bpm")] $
    M.textNode "marquee marquee marquee netscape navigator"
  M.voidNode (Tag "img") [Attribute (AttributeKey "src") (AttributeValue "google.com")]
  M.parentNode (Tag "p") [] $
    M.textNode "html is for you and me"

mlinear :: Int -> M.Html ()
mlinear n =
  replicateM_ n mthing

mnested :: Int -> M.Html ()
mnested 0 =
  mthing
mnested n = do
  M.parentNode (Tag "div") [Attribute (AttributeKey "blink") (AttributeValue "210bpm")] $
    mthing *> mnested (n-1)

-- -----------------------------------------------------------------------------
-- Blaze

bthing :: Blaze.Html
bthing =
  mconcat [
      Blaze.docType
    , Blaze.div
        (Blaze.text "marquee marquee marquee netscape navigator")
          Blaze.! Blaze.customAttribute "blink" "160bpm"
    , Blaze.img
        Blaze.! Blaze.customAttribute "src" "google.com"
    , Blaze.p (Blaze.text "html is for you and me")
    ]

blinear :: Int -> Blaze.Html
blinear n =
  mconcat (L.replicate n bthing)

bnested :: Int -> Blaze.Html
bnested 0 =
  bthing
bnested n =
  Blaze.div (bthing <> bnested (n-1))
    Blaze.! Blaze.customAttribute "blink" "210bpm"

bToText :: Blaze.Html -> Text
bToText =
  TL.toStrict . Blaze.renderHtml

-- -----------------------------------------------------------------------------
-- Lucid

lthing :: Lucid.Html ()
lthing =
  mconcat [
      Lucid.doctype_
    , Lucid.div_
        [Lucid.Attribute "blink" "160bpm"]
        (Lucid.toHtml ("marquee marquee marquee netscape navigator" :: Text))
    , Lucid.img_ [Lucid.Attribute "src" "google.com"]
    , Lucid.p_ (Lucid.toHtml ("html is for you and me" :: Text))
    ]

llinear :: Int -> Lucid.Html ()
llinear n =
  mconcat (L.replicate n lthing)

lnested :: Int -> Lucid.Html ()
lnested 0 =
  lthing
lnested n =
  Lucid.div_
    [Lucid.Attribute "blink" "210bpm"]
    (lthing <> lnested (n-1))

lToText :: Lucid.Html () -> Text
lToText =
  TL.toStrict . Lucid.renderText

-- -----------------------------------------------------------------------------

main :: IO ()
main = do
  let cfg =
        defaultConfig {
            reportFile = Just "dist/build/hydrant-bench.html"
          , csvFile    = Just "dist/build/hydrant-bench.csv"
          }
      go f = nf toText . f
      bgo f = nf bToText . f
      lgo f = nf lToText . f
      mgo f = nf toText . M.runHtml . f
  glass <- T.readFile "test/glass.txt"
  defaultMainWith cfg [
      bgroup "linear" [
          bench "hydrant-linear-100" (go linear 100)
        , bench "hydrant-linear-200" (go linear 200)
        , bench "hydrant-linear-500" (go linear 500)
        , bench "hydrant-linear-1000" (go linear 1000)
        , bench "hydrant-monadic-linear-100" (mgo mlinear 100)
        , bench "hydrant-monadic-linear-200" (mgo mlinear 200)
        , bench "hydrant-monadic-linear-500" (mgo mlinear 500)
        , bench "hydrant-monadic-linear-1000" (mgo mlinear 1000)
        , bench "lucid-linear-100" (lgo llinear 100)
        , bench "lucid-linear-200" (lgo llinear 200)
        , bench "lucid-linear-500" (lgo llinear 500)
        , bench "lucid-linear-1000" (lgo llinear 1000)
        , bench "blaze-linear-100" (bgo  blinear 100)
        , bench "blaze-linear-200" (bgo  blinear 200)
        , bench "blaze-linear-500" (bgo  blinear 500)
        , bench "blaze-linear-1000" (bgo blinear 1000)
        ]
    , bgroup "nested" [
          bench "hydrant-nested-100" (go nested 100)
        , bench "hydrant-nested-200" (go nested 200)
        , bench "hydrant-nested-500" (go nested 500)
        , bench "hydrant-nested-1000" (go nested 1000)
        , bench "hydrant-monadic-nested-100" (mgo mnested 100)
        , bench "hydrant-monadic-nested-200" (mgo mnested 200)
        , bench "hydrant-monadic-nested-500" (mgo mnested 500)
        , bench "hydrant-monadic-nested-1000" (mgo mnested 1000)
        , bench "lucid-nested-100"  (lgo lnested 100)
        , bench "lucid-nested-200"  (lgo lnested 200)
        , bench "lucid-nested-500"  (lgo lnested 500)
        , bench "lucid-nested-1000" (lgo lnested 1000)
        , bench "blaze-nested-100"  (bgo bnested 100)
        , bench "blaze-nested-200"  (bgo bnested 200)
        , bench "blaze-nested-500"  (bgo bnested 500)
        , bench "blaze-nested-1000" (bgo bnested 1000)
        ]
    , bgroup "escaping" [
          bench "escaping-100" (nf (escape glass) 100)
        , bench "escaping-200" (nf (escape glass) 200)
        , bench "escaping-500" (nf (escape glass) 500)
        , bench "escaping-1000" (nf (escape glass) 1000)
        ]
    ]
