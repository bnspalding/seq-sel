module Annotations where

import qualified Data.Set as Set
import qualified Data.Text as T
import Dictionary
import Poem
import Sound
import Sound.Word (symbols)

-- TODO: integrate new annotations into old gen process

data Annotation
  = IPA [[[T.Text]]] -- lines and stanzas are preserved
  | Stress [[[[Stress]]]] -- lines and stanzas are preserved
  | Definitions [(T.Text, [Definition])] -- lines and stanzas are NOT preserved

-- | mkAnnotationIPA generates an annotation of IPA pronunciations for the
-- entries of a poem.
mkAnnotationIPA :: Poem -> Annotation
mkAnnotationIPA =
  IPA . fmap (fmap (fmap (symbols . pronunciation)))

-- NOTE: It may make sense to change the way that Word.symbols (from Sound)
-- works to include stress and syllable breaks more clearly

-- | mkAnnotationStress generates an annotation of syllable stresses for the
-- entries of a poem.
mkAnnotationStress :: Poem -> Annotation
mkAnnotationStress = Stress . fmap (fmap (fmap (fmap stress . pronunciation)))

-- | mkAnnotationDefs generates an annotation containing a list of all definitions
-- for all entries in a poem.
--
-- Each entry is transformed into a pair of form (Word, [Definitions])
mkAnnotationDefs :: Poem -> Annotation
mkAnnotationDefs = Definitions . concatMap (concatMap (fmap mkPairs))
  where
    mkPairs e = (text e, Set.toList (definitions e))
-- additional annotations:
-- distance (the number of words skipped over between two words)
-- rhyme strength (actual similarity numbers of rhymes)
--
-- Note: some of these may benefit from the addition of a Record element to the
-- spec
