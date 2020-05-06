module Annotations where

import qualified Data.Text as T
import Dictionary
import Poem
import Sound
import Sound.Syl (sounds)

-- TODO: move annotation generation here
-- TODO: create a type for Annotations

data Annotation
  = IPA [[[T.Text]]] -- lines and stanzas are preserved
  | Stress [[[Stress]]] -- lines and stanzas are preserved
  | Definitions [T.Text] -- lines and stanzas are NOT preserved

-- | mkAnnotationIPA generates an annotation of IPA pronunciations for the
-- entries of a poem.
mkAnnotationIPA :: Poem -> Annotation
mkAnnotationIPA = IPA . fmap (fmap (fmap entryToIPA))
  where
    entryToIPA =
      T.concat
        . fmap (\(Sound s) -> s)
        . concatMap sounds
        . pronunciation

-- | mkAnnotationStress generates an annotation of syllable stresses for the
-- entries of a poem.
mkAnnotationStress :: Poem -> Annotation
mkAnnotationStress = undefined

-- | mkAnnotationDefs generates an annotation containing a list of definitions
-- for all entries in a poem.
mkAnnotationDefs :: Poem -> Annotation
mkAnnotationDefs = undefined
