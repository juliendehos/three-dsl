-----------------------------------------------------------------------------
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
module THREE.Mesh
  ( -- * Types
    Mesh (..)
    -- * Constructors
  , THREE.Mesh.new
    -- * Read-only Properties
    -- * Properties
    -- * Optional properties
    -- * Methods
    -- * Helper functions
  ) where
-----------------------------------------------------------------------------
import           Language.Javascript.JSaddle hiding (new)
-----------------------------------------------------------------------------
import           THREE.BufferGeometry (BufferGeometryClass)
import qualified THREE.Internal as THREE
import           THREE.Material as THREE
import           THREE.Object3D as THREE
import           THREE.EventDispatcher as THREE
-----------------------------------------------------------------------------
-- | https://threejs.org/docs/#api/en/objects/Mesh
newtype Mesh
  = Mesh
  { unMesh :: JSVal
  } -- deriving (MakeArgs, MakeObject, ToJSVal) 
    -- deriving (EventDispatcher)
    -- deriving anyclass Object3D
    deriving newtype (MakeArgs, MakeObject, ToJSVal)
    deriving anyclass (Object3D, EventDispatcher)


-- instance Object3D Mesh

-----------------------------------------------------------------------------
new
  :: (BufferGeometryClass geometry, Material material)
  => geometry
  -> material
  -> THREE.Three Mesh
new geometry material = THREE.new Mesh "Mesh" (geometry, material)
-----------------------------------------------------------------------------
