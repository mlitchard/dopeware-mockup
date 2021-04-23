{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE JavaScriptFFI       #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE TypeFamilies        #-}

module JavaScript where

import           Control.Monad.IO.Class
-- import           Data.JSString               (JSString)
-- import qualified Data.JSString               as JSString
import           Language.Javascript.JSaddle
-- import           Reflex.Dom                  hiding (Window)


setBubble :: forall m . (MonadIO m) => m (Maybe Int)
setBubble = liftIO $ do
    val' <- js_setBubble
    fromJSVal val'


foreign import javascript unsafe
    "setBubble()"
    js_setBubble :: IO JSVal

