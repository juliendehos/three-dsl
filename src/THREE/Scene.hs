-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
module THREE.Scene
  ( -- * Types
    Scene (..)
    -- * Constructors
  , THREE.Scene.new
    -- * Read-only Properties
  , isScene
    -- * Properties
    -- * Optional properties
    -- * Methods
    -- * Helper functions
  ) where
-----------------------------------------------------------------------------
import           Language.Javascript.JSaddle
-----------------------------------------------------------------------------
import           THREE.Internal        as THREE
import           THREE.Object3D        as THREE
import           THREE.EventDispatcher as THREE
-----------------------------------------------------------------------------
-- | https://threejs.org/docs/#api/en/scenes/Scene
newtype Scene
  = Scene
  { unScene :: JSVal
  } deriving newtype (MakeArgs, MakeObject, ToJSVal)
    deriving anyclass (Object3D, EventDispatcher)

instance FromJSVal Scene where
  fromJSVal = pure . Just . Scene
-----------------------------------------------------------------------------
new :: THREE.Three Scene
new = THREE.new Scene "Scene" ()
-----------------------------------------------------------------------------
isScene :: ReadOnly Scene "isScene" Bool
isScene = readonly
-----------------------------------------------------------------------------
