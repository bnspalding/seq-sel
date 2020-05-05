module Spec
  ( Spec (..),
    Resources (..),
    resources,
    state,
  )
where

import Dictionary
import Selection.Constraints (PoemState)

newtype Resources = Resources {dict :: Dictionary}

data Spec = Spec PoemState Resources

resources :: Spec -> Resources
resources (Spec _ res) = res

state :: Spec -> PoemState
state (Spec st _) = st
