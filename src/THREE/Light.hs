-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeApplications #-}
-----------------------------------------------------------------------------
module THREE.Light
  ( -- * Types
    Light (..)
    -- * Constructors
    -- * Read-only Properties
    -- * Properties
    -- * Optional properties
    -- * Methods
    -- * Helper functions
  ) where
-----------------------------------------------------------------------------
import           Data.Proxy (Proxy(Proxy))
import           Language.Javascript.JSaddle
-----------------------------------------------------------------------------
import           THREE.Color as THREE
import           THREE.Internal as THREE
import           THREE.Object3D as THREE
-----------------------------------------------------------------------------
-- | https://threejs.org/docs/#api/en/lights/Light
class Object3D light => Light light where
  -- read-only properties
  isLight :: Property light "isLight" Bool
  -- properties
  color :: Property light "color" Color
  intensity :: Property light "intensity" Double
  -- optional properties
  -- methods
  copy :: (MakeArgs a, Light a) => light -> a -> THREE.Three ()
  dispose :: light -> () -> THREE.Three ()
-----------------------------------------------------------------------------
instance Light JSVal where
  -- read-only properties
  isLight = property
  -- properties
  color = property
  intensity = property
  -- optional properties
  -- methods
  copy = method (Proxy @"copy")
  dispose = method (Proxy @"dispose")
-----------------------------------------------------------------------------
