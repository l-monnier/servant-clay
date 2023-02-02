{-# LANGUAGE DeriveLift            #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

-- | Template Haskell related functions and type classes.
--
-- It allows to render and convert Clay `Css` to lazy `ByteString` at compile
-- time.
--
-- You define your Clay `Css`, for example:
--
-- > myCss :: Css
-- > myCss = body ? color blue
--
-- Then, in another module, with the Template Haskell language extension
-- enabled, you can use a splice such as:
--
-- > myRenderedCss :: ByteString
-- > myRenderedCss = $(mkCssQ myCss)
--
-- As the type constructor of CssTh is not exported, it is not possible
-- to construct this type in an other way than using Template Haskell
-- with mkCssQ which only accepts a Clay `Css` type argument.
module Servant.CSS.Clay.TH
  ( CSSTH
  , CssTh
  , mkCssQ
  ) where

import Clay (Css, render)
import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Language.Haskell.TH.Syntax (Q, Exp, Lift, lift)
import Network.HTTP.Media ((//), (/:))
import Servant.API (Accept (..), MimeRender (..))

-- | CSS Template Haskell type for Servant.
data CSSTH

-- | @text/css;charset=utf-8@
instance Accept CSSTH where
    contentType _ = "text" // "css" /: ("charset", "utf-8")

instance MimeRender CSSTH CssTh where
    mimeRender _ = runCssTh

-- | A newtype around a lazy `ByteString`.
-- The constructor is not exported.
-- It can only be built using the Template Haskell constructor `mkCssQ`.
newtype CssTh = CssTh { runCssTh :: ByteString } deriving (Eq, Lift, Ord, Show)

-- | Makes a Template Haskell `CssTH` expression.
-- This constructor is to be used in a splice:
--
-- > $(mkCssQ myCss)
--
-- where myCss is your `Css` created with Clay.
-- Note that due to Template Haskell limitations, it must be hosted in another
-- module.
mkCssQ :: Css -> Q Exp
mkCssQ css = lift . CssTh . encodeUtf8 $ render css
