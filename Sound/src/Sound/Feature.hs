module Sound.Feature where

import Data.Set as Set

contains :: FeatureSet -> FeatureSet -> Bool
contains = Set.isSubsetOf

contains1 :: Feature -> FeatureSet -> Bool
contains1 = Set.member

featuresOrEmpty :: Maybe FeatureSet -> FeatureSet
featuresOrEmpty (Just fs) = fs
featuresOrEmpty Nothing = Set.empty

data Feature
  = PLUS_SYLLABIC
  | MINUS_SYLLABIC
  | PLUS_CONSONANTAL -- vocal tract constriction
  | MINUS_CONSONANTAL -- "
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
  deriving (Eq, Ord, Show)

type FeatureSet = Set Feature

featureSet :: [Feature] -> Set Feature
featureSet = Set.fromList

isStop :: FeatureSet -> Bool
isStop =
  ((&&) . contains (featureSet [MINUS_SONORANT, MINUS_CONTINUANT])) <*>
  (not . contains1 DELREL)

isVoiced :: FeatureSet -> Bool
isVoiced = contains1 PLUS_VOICE

isFricative :: FeatureSet -> Bool
isFricative = contains (featureSet [PLUS_CONTINUANT, MINUS_SONORANT])

isAffricate :: FeatureSet -> Bool
isAffricate = contains1 DELREL

isNasal :: FeatureSet -> Bool
isNasal = contains1 NASAL

isLateral :: FeatureSet -> Bool
isLateral = contains1 LATERAL

isApproximant :: FeatureSet -> Bool
isApproximant =
  contains (featureSet [PLUS_CONTINUANT, PLUS_SONORANT, MINUS_SYLLABIC])

isGlide :: FeatureSet -> Bool
isGlide = contains (featureSet [MINUS_CONSONANTAL, MINUS_SYLLABIC])

isVowel :: FeatureSet -> Bool
isVowel = contains1 PLUS_SYLLABIC

isHighVowel :: FeatureSet -> Bool
isHighVowel = contains (featureSet [PLUS_SYLLABIC, PLUS_HIGH])

isMidVowel :: FeatureSet -> Bool
isMidVowel = contains (featureSet [PLUS_SYLLABIC, MINUS_HIGH, MINUS_LOW])

isLowVowel :: FeatureSet -> Bool
isLowVowel = contains (featureSet [PLUS_SYLLABIC, PLUS_LOW])
