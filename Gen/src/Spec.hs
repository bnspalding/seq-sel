module Spec (Spec (..)) where

import Selection.Constraints (Spec (..))

-- NOTE: The Spec is tied very tightly to Selection.Constraints. I would like to
-- disentangle it more, but for now I'm just reexporting it. There should be a
-- way to smooth out these dependencies however, such that Selection.Constraints
-- can worry only about the PoemCons, and not the entire Spec.
