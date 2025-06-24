{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module DSL where

import Control.Monad (ap, liftM, void)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Kind (Type)
import Language.Javascript.JSaddle as J

-------------------------------------------------------------------------------
-- Three types
-------------------------------------------------------------------------------

newtype Scene = Scene { unScene :: JSVal }
  deriving (MakeArgs, MakeObject, ToJSVal) 

newtype PointLight = PointLight { unPointLight :: JSVal }
  deriving (MakeArgs, MakeObject, ToJSVal) 

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
  Add' :: (MakeArgs a) => Scene -> a -> Three ()

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

add :: (MakeArgs a) => Scene -> a -> Three ()
add = Add'

-------------------------------------------------------------------------------
-- interpreter
-------------------------------------------------------------------------------

new' :: MakeArgs a => (JSVal -> b) -> JSString -> a -> JSM b
new' f name args = do
  v <- jsg ("THREE" :: JSString) ! name
  f <$> J.new v args

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

interpret (Add' s x) =
  void $ unScene s # ("add" :: JSString) $ x


