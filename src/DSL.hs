{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module DSL where

import Control.Monad (ap, liftM)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Kind (Type)
import Language.Javascript.JSaddle as J

import API

-------------------------------------------------------------------------------
-- Three GADT
-------------------------------------------------------------------------------

data Three :: Type -> Type where
  Pure :: a -> Three a
  Bind :: Three a -> (a -> Three b) -> Three b
  LiftIO :: IO a -> Three a
  LiftJSM :: JSM a -> Three a

  Scene' :: Three Scene
  PointLight' :: Three PointLight
  Add' :: (Object3DC a, Object3DC b, MakeArgs b) => a -> b -> Three ()
  GetIntensityLight :: LightC a => a -> Three Double
  SetIntensityLight :: LightC a => a -> Double -> Three ()

instance Functor Three where
  fmap = liftM

instance Applicative Three where
  (<*>) = ap
  pure = Pure

instance Monad Three where
  (>>=) = Bind
  return = pure

instance MonadIO Three where
  liftIO = LiftIO

instance MonadJSM Three where
  liftJSM' = LiftJSM

-------------------------------------------------------------------------------
-- helpers
-------------------------------------------------------------------------------

scene :: Three Scene
scene = Scene'

pointLight :: Three PointLight
pointLight = PointLight'

add :: (Object3DC a, Object3DC b, MakeArgs b) => a -> b -> Three ()
add = Add'

getIntensityLight :: LightC a => a -> Three Double
getIntensityLight = GetIntensityLight

setIntensityLight :: LightC a => a -> Double -> Three ()
setIntensityLight = SetIntensityLight

-------------------------------------------------------------------------------
-- interpreter
-------------------------------------------------------------------------------

interpret :: Three a -> JSM a

interpret (Pure m) =
  pure m

interpret (Bind m f) =
  interpret m >>= interpret . f

interpret (LiftIO io) =
  liftIO io

interpret (LiftJSM jsm) =
  liftJSM jsm

interpret Scene' =
  new' Scene "Scene" ()

interpret PointLight' =
  new' PointLight "PointLight" ()

interpret (Add' v x) =
  API.add v x

interpret (GetIntensityLight v) =
  API.getIntensity v

interpret (SetIntensityLight v x) =
  API.setIntensity x v


