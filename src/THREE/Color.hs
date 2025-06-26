-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
module THREE.Color
  ( -- * Types
    Color (..)
  , Params (..)
    -- * Methods
  , THREE.Color.new
    -- * Properties
  , def
  ) where
-----------------------------------------------------------------------------
import           Language.Javascript.JSaddle
-----------------------------------------------------------------------------
import qualified THREE.Internal as THREE
-----------------------------------------------------------------------------
-- | https://threejs.org/docs/index.html#api/en/math/Color
newtype Color
  = Color
  { unColor :: JSVal
  } deriving (MakeObject, ToJSVal, MakeArgs)
-----------------------------------------------------------------------------
instance FromJSVal Color where
  fromJSVal = pure .Just . Color
-----------------------------------------------------------------------------
data Params 
  = Rgb
      { r_ :: Double
      , g_ :: Double
      , b_ :: Double
      } 
  | Hex Int
  | Str String
-----------------------------------------------------------------------------
def :: Params
def = Rgb 1 1 1
-----------------------------------------------------------------------------
new :: Params -> THREE.Three Color
new (Rgb r g b) = THREE.new Color "Color" (r, g, b)
new (Hex h) = THREE.new Color "Color" [h]
new (Str s) = THREE.new Color "Color" [s]
-----------------------------------------------------------------------------
