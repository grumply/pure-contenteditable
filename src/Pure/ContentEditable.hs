{-# language RankNTypes, ScopedTypeVariables, LambdaCase, BlockArguments, TypeApplications, NamedFieldPuns, MultiParamTypeClasses, OverloadedStrings, ImplicitParams, PatternSynonyms, TemplateHaskell, ViewPatterns, TypeFamilies, FlexibleContexts #-}
module Pure.ContentEditable 
  ( ContentEditable(..)
  , asDiv
  , asP
  , pattern ContentEditable
  , pattern OnStartup
  , pattern OnShutdown
  ) where

import Pure.Elm.Component hiding (pattern ContentEditable,children,features,start)
import Pure.Data.Prop.TH
import Pure.Data.View (Pure(..))

import Control.Monad (void)
import Data.Coerce (coerce)
import Data.Unique (newUnique,hashUnique)

import Data.IORef (IORef,newIORef,atomicModifyIORef')
import System.IO.Unsafe (unsafePerformIO)

{-
This implementation isn't very friendly. If you use this module, prepare to curse it.
-}

data ContentEditable = ContentEditable_
  { as         :: Features -> [View] -> View
  , children   :: [View]
  , features   :: Features
  , onStartup  :: ([View] -> (Node -> IO ()) -> IO ()) -> IO () 
  , onShutdown :: IO ()
  }
deriveLocalComponent ''ContentEditable

asDiv :: Features -> [View] -> View
asDiv fs cs = Children cs (Features fs Div)

asP :: Features -> [View] -> View
asP fs cs = Children cs (Features fs P)

instance Default ContentEditable where
  def = ContentEditable_ asDiv def def def def

instance Pure ContentEditable where
  view = run

instance Component ContentEditable where
  data Model ContentEditable = Model
    { node  :: Node 
    , write :: (Bool,[View])
    , ident :: Int
    }

  initialize ce = do
    u <- hashUnique <$> newUnique
    pure (Model (Node def) (False,children ce) u)

  data Msg ContentEditable = SetHost Node | SetContent [View] | Shutdown

  shutdown = [Shutdown]

  upon = \case
    SetHost h    -> setHost h
    SetContent c -> setContent c
    Shutdown     -> stop

  view ContentEditable_ { as, features, children } Model { node, write = (switch,live), ident } = 
    -- Use `Tagged @()` as a hack to force full re-renders rather than partial 
    -- re-renders that aren't aware of the contenteditable nature of the view.
    -- This update strategy defeats much of the reconciliation optimization that
    -- Pure.hs tries to accomplish.
    flip lazy switch $ \_ ->
      (if switch then Tagged @() else id) $
        (as features live) 
          <| Host node (command . SetHost) 
           . Id ("pure_ce_" <> toTxt ident)
           . Attribute "contenteditable" "true" 

stop :: Update ContentEditable
stop ContentEditable_ { onShutdown } mdl = do
  onShutdown
  pure mdl

setHost :: Node -> Update ContentEditable
setHost node ContentEditable_ { onStartup } mdl@Model { ident } = do
  onStartup (\cnt f -> commandWith (SetContent cnt) (f node))
  pure mdl { node = node }

setContent :: [View] -> Update ContentEditable
setContent p _cfg mdl@Model { write = (switch,_) } = do
  pure mdl
    { write = (Prelude.not switch,p)
    }