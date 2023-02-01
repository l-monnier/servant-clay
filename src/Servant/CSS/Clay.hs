{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

-- | A @CSS@ empty data type with `MimeRender` instances for @Clay@'s
-- `Css` datatype.
-- A `ToCss` class converting to @Clay@'s `Css` is also provided
-- with an instance for @Clay@'s `Css` in case all your CSS is defined
-- in one place.
-- You should only need to import this module for it's instances and the
-- `CSS` datatype.:
--
-- >>> type Eg = Get '[CSS] a
--
-- Will then check that @a@ has a `ToCss` instance.
module Servant.CSS.Clay
  ( CSS
  , ToCss
  , toCss
  )
where

import Clay (Css, render)
import Data.Text (Text)
import Data.Text.Lazy (fromStrict)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Network.HTTP.Media ((//), (/:))
import Servant.API (Accept (..), MimeRender (..))

-- | CSS data type for Servant.
data CSS

-- | @text/css;charset=utf-8@
instance Accept CSS where
    contentType _ = "text" // "css" /: ("charset", "utf-8")

instance ToCss a => MimeRender CSS a where
    mimeRender _ = encodeUtf8 . render. toCss

instance MimeRender CSS Text where
    mimeRender _ = encodeUtf8 . fromStrict

-- | Conversion to Clay `Css`.
class ToCss a where
  toCss :: a -> Css

instance ToCss Css where
  toCss x = x
