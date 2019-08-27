module Feature where

import Data.Set as Set

data Feature
  = PLUS_SYLLABIC
  | MINUS_SYLLABIC
  | PLUS_CONSONANTAL -- vocal tract constriction
  | MINUS_CONSONANTAL -- "
  | PLUS_APPROX
  | MINUS_APPROX
  | PLUS_SONORANT -- pressure behind oral constriction
  | MINUS_SONORANT -- "
  | PLUS_CONTINUANT -- complete closure of oral cavity
  | MINUS_CONTINUANT -- "
  | NASAL -- articulated with open velum
  | LATERAL -- vocal tract closed at center, open at sides
  | DELREL -- delayed release (affricates)
  | PLUS_VOICE -- sounds that have vibrating vocal folds
  | MINUS_VOICE -- "
  | SG -- spread glottis (large glottal opening gesture)
  | CG -- constricted glottis (constricted vocal folds)
  | LABIAL -- constriction at lips
  | CORONAL -- constriction made with the tongue front
  | DORSAL -- constriction made with the tongue body
  | PHARYNGEAL -- constriction made with tongue root
  | LARYNGEAL -- constriction made at the glottis
  | PLUS_ANTERIOR -- tongue at front of alveolar ridge
  | MINUS_ANTERIOR -- tongue behind alveolar ridge
  | PLUS_DISTRIB -- relatively long constriction
  | MINUS_DISTRIB -- "
  | PLUS_STRIDENT -- high-amplitude, high-pitched fricative
  | MINUS_STRIDENT -- "
  | PLUS_ROUND -- vowels: pursing of the lips
  | MINUS_ROUND -- "
  | PLUS_HIGH -- vowels: tongue is raised above neutral
  | MINUS_HIGH -- "
  | PLUS_LOW -- vowels: tongue is lowered below neutral
  | MINUS_LOW -- "
  | PLUS_BACK -- vowels: tongue is moved back from neutral
  | MINUS_BACK -- "
  | PLUS_ATR -- vowels: tongue root is pulled forward
  | MINUS_ATR -- "
  | PLUS_WIDE -- diphthongs: wide vs narrow
  | MINUS_WIDE -- "
  | RHOTIC -- r colored vowels
  | PLUS_STRESSED -- differentiating between rhotic ə and ɜ (tenuous)
  | MINUS_STRESSED -- "

type FeatureSet = Set Feature
