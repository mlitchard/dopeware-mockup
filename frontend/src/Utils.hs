{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE LambdaCase          #-}

module Utils where

import           Control.Monad.Fix


    
    -- Cribbed from the ChatWisely code-base.
    -- Written by Brian Hurt
knot :: MonadFix m => (a -> m (a, b)) -> m b
knot f = do
    x <- mfix $ \y -> do
                        let ~(a, _) = y
                        f a
    let ~(_, b) = x
    return b




