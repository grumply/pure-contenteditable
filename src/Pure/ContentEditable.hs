{-# language RankNTypes, ScopedTypeVariables, LambdaCase, BlockArguments, TypeApplications, NamedFieldPuns, MultiParamTypeClasses, OverloadedStrings, ImplicitParams, PatternSynonyms #-}
module Pure.ContentEditable (ContentEditable(..), contentEditable) where

import Pure.Elm hiding (pattern ContentEditable,children,features)

import Control.Monad (void)

data ContentEditable = ContentEditable
  { as         :: Features -> [View] -> View
  , children   :: [View]
  , features   :: Features
  , onStartup  :: ([View] -> IO () -> IO ()) -> IO () 
  , onShutdown :: IO ()
  }

instance HasFeatures ContentEditable where
  getFeatures = features
  setFeatures fs ce = ce { features = fs }

instance HasChildren ContentEditable where
  getChildren = children
  setChildren cs ce = ce { children = cs }

data Model = Model
  { node  :: Node 
  , write :: (Bool,[View])
  }

data Msg = Startup | Host Node | SetContent [View] | Shutdown

type Update = Elm Msg => ContentEditable -> Model -> IO Model

contentEditable :: ContentEditable -> View
contentEditable c = run (Applet [Startup] [] [Shutdown] (pure model) upon view) c
  where model = Model (Node def) (False,children c)

upon :: Msg -> Update
upon = \case
  Startup      -> startup
  Host h       -> host h
  SetContent c -> setContent c
  Shutdown     -> shutdown

startup :: Update
startup ContentEditable { onStartup } mdl = do
  onStartup (\cnt f -> void (?command (SetContent cnt) f))
  pure mdl

shutdown :: Update
shutdown ContentEditable { onShutdown } mdl = do
  onShutdown
  pure mdl

host :: Node -> Update
host node _ mdl = 
  pure mdl 
    { node = node }

setContent :: [View] -> Update
setContent p _cfg mdl@Model { write = (switch,_) } = do
  pure mdl
    { write = (Prelude.not switch,p)
    }

type Render = Elm Msg => Model -> View

view :: ContentEditable -> Render 
view ContentEditable { as, features, children } Model { write = (switch,live) } = 
  -- Use `Tagged @()` as a hack to force full re-renders rather than partial 
  -- re-renders that aren't aware of the contenteditable nature of the view.
  -- This update strategy defeats much of the reconciliation optimization that
  -- Pure.hs tries to accomplish.
  (if switch then Tagged @() else id) $
    (as features live) 
      <| WithHost (command . Host) 
       . Attribute "contenteditable" "true" 
