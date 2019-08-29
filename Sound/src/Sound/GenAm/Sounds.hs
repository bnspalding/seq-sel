module Sound.GenAm.Sounds where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Sound.Feature
import Sound.Sound

--- Private: GenAm definitions ----------------
_sounds :: Map.Map Sound FeatureSet
_sounds = Map.union consonants vowels

consonants :: Map.Map Sound FeatureSet
consonants =
  Map.fromList
    [ ( Sound "m" -- vd bilabial nasal
      , Set.fromList
          [ MINUS_SYLLABIC
          , PLUS_CONSONANTAL
          , PLUS_SONORANT
          , MINUS_CONTINUANT
          , NASAL
          , PLUS_VOICE
          , LABIAL
          ])
    , ( Sound "n" -- vd alveolar nasal
      , Set.fromList
          [ MINUS_SYLLABIC
          , PLUS_CONSONANTAL
          , PLUS_SONORANT
          , MINUS_CONTINUANT
          , NASAL
          , PLUS_VOICE
          , CORONAL
          , PLUS_ANTERIOR
          , MINUS_DISTRIB
          ])
    , ( Sound "ŋ" -- 014B vd velar nasal
      , Set.fromList
          [ MINUS_SYLLABIC
          , PLUS_CONSONANTAL
          , PLUS_SONORANT
          , MINUS_CONTINUANT
          , NASAL
          , PLUS_VOICE
          , DORSAL
          ])
    , ( Sound "p" -- 0070 vl bilabial stop
      , Set.fromList
          [ MINUS_SYLLABIC
          , PLUS_CONSONANTAL
          , MINUS_SONORANT
          , MINUS_CONTINUANT
          , MINUS_VOICE
          , LABIAL
          ])
    , ( Sound "b" -- 0062 vd bilabial stop
      , Set.fromList
          [ MINUS_SYLLABIC
          , PLUS_CONSONANTAL
          , MINUS_SONORANT
          , MINUS_CONTINUANT
          , PLUS_VOICE
          , LABIAL
          ])
    , ( Sound "t" -- 0074 vl alveolar stop
      , Set.fromList
          [ MINUS_SYLLABIC
          , PLUS_CONSONANTAL
          , MINUS_SONORANT
          , MINUS_CONTINUANT
          , MINUS_VOICE
          , CORONAL
          , PLUS_ANTERIOR
          , MINUS_DISTRIB
          ])
    , ( Sound "d" -- 0064 vd alveolar stop
      , Set.fromList
          [ MINUS_SYLLABIC
          , PLUS_CONSONANTAL
          , MINUS_SONORANT
          , MINUS_CONTINUANT
          , PLUS_VOICE
          , CORONAL
          , PLUS_ANTERIOR
          , MINUS_DISTRIB
          ])
    , ( Sound "k" -- 006B vl velar stop
      , Set.fromList
          [ MINUS_SYLLABIC
          , PLUS_CONSONANTAL
          , MINUS_SONORANT
          , MINUS_CONTINUANT
          , MINUS_VOICE
          , DORSAL
          ])
    , ( Sound "ɡ" -- 0261 vd velar stop
      , Set.fromList
          [ MINUS_SYLLABIC
          , PLUS_CONSONANTAL
          , MINUS_SONORANT
          , MINUS_CONTINUANT
          , PLUS_VOICE
          , DORSAL
          ])
    , ( Sound "t͡ʃ" -- 0074 0361 0283 vl postalveolar affricate
      , Set.fromList
          [ MINUS_SYLLABIC
          , PLUS_CONSONANTAL
          , MINUS_SONORANT
          , MINUS_CONTINUANT
          , DELREL
          , MINUS_VOICE
          , CORONAL
          , MINUS_ANTERIOR
          , MINUS_DISTRIB
          , PLUS_STRIDENT
          ])
    , ( Sound "d͡ʒ" -- 0064 0361 0292, vd postalveolar affricate
      , Set.fromList
          [ MINUS_SYLLABIC
          , PLUS_CONSONANTAL
          , MINUS_SONORANT
          , MINUS_CONTINUANT
          , DELREL
          , PLUS_VOICE
          , CORONAL
          , MINUS_ANTERIOR
          , MINUS_DISTRIB
          , PLUS_STRIDENT
          ])
    , ( Sound "f" -- 0066, vl labiodental fricative,
      , Set.fromList
          [ MINUS_SYLLABIC
          , PLUS_CONSONANTAL
          , MINUS_SONORANT
          , PLUS_CONTINUANT
          , MINUS_VOICE
          , LABIAL
          , PLUS_STRIDENT
          ])
    , ( Sound "v" -- 0076, vd labiodental fricative
      , Set.fromList
          [ MINUS_SYLLABIC
          , PLUS_CONSONANTAL
          , MINUS_SONORANT
          , PLUS_CONTINUANT
          , PLUS_VOICE
          , LABIAL
          , PLUS_STRIDENT
          ])
    , ( Sound "θ" -- 03B8, vl dental fricative,
      , Set.fromList
          [ MINUS_SYLLABIC
          , PLUS_CONSONANTAL
          , MINUS_SONORANT
          , PLUS_CONTINUANT
          , MINUS_VOICE
          , CORONAL
          , PLUS_ANTERIOR
          , PLUS_DISTRIB
          , MINUS_STRIDENT
          ])
    , ( Sound "ð" -- 00F0, vd dental fricative
      , Set.fromList
          [ MINUS_SYLLABIC
          , PLUS_CONSONANTAL
          , MINUS_SONORANT
          , PLUS_CONTINUANT
          , PLUS_VOICE
          , CORONAL
          , PLUS_ANTERIOR
          , PLUS_DISTRIB
          , MINUS_STRIDENT
          ])
    , ( Sound "s" -- 0073, vl alveolar fricative,
      , Set.fromList
          [ MINUS_SYLLABIC
          , PLUS_CONSONANTAL
          , MINUS_SONORANT
          , PLUS_CONTINUANT
          , MINUS_VOICE
          , CORONAL
          , PLUS_ANTERIOR
          , MINUS_DISTRIB
          , PLUS_STRIDENT
          ])
    , ( Sound "z" -- 007A, vd alveolar fricative
      , Set.fromList
          [ MINUS_SYLLABIC
          , PLUS_CONSONANTAL
          , MINUS_SONORANT
          , PLUS_CONTINUANT
          , PLUS_VOICE
          , CORONAL
          , PLUS_ANTERIOR
          , MINUS_DISTRIB
          , PLUS_STRIDENT
          ])
    , ( Sound "ʃ" -- 0283, vl postalveolar fricative,
      , Set.fromList
          [ MINUS_SYLLABIC
          , PLUS_CONSONANTAL
          , MINUS_SONORANT
          , PLUS_CONTINUANT
          , MINUS_VOICE
          , CORONAL
          , MINUS_ANTERIOR
          , PLUS_DISTRIB
          , PLUS_STRIDENT
          ])
    , ( Sound "ʒ" -- 0292, vd postalveolar fricative
      , Set.fromList
          [ MINUS_SYLLABIC
          , PLUS_CONSONANTAL
          , MINUS_SONORANT
          , PLUS_CONTINUANT
          , PLUS_VOICE
          , CORONAL
          , MINUS_ANTERIOR
          , PLUS_DISTRIB
          , PLUS_STRIDENT
          ])
    , ( Sound "h" -- 0068, vl glottal fricative,
      , Set.fromList
          [ MINUS_SYLLABIC
          , PLUS_CONSONANTAL
          , MINUS_SONORANT
          , PLUS_CONTINUANT
          , MINUS_VOICE
          , LARYNGEAL
          , MINUS_STRIDENT
          ])
    , ( Sound "l" -- 006C, vd alveolar lateral approximant,
      , Set.fromList
          [ MINUS_SYLLABIC
          , PLUS_CONSONANTAL
          , PLUS_SONORANT
          , PLUS_CONTINUANT
          , LATERAL
          , PLUS_VOICE
          , CORONAL
          , PLUS_ANTERIOR
          , PLUS_DISTRIB
          ])
    , ( Sound "ɹ" -- 0279, vd alveolar approximant
      , Set.fromList
          [ MINUS_SYLLABIC
          , PLUS_CONSONANTAL
          , PLUS_SONORANT
          , PLUS_CONTINUANT
          , PLUS_VOICE
          , CORONAL
          , PLUS_ANTERIOR
          , PLUS_DISTRIB
          ])
    , ( Sound "j" -- 006A, vd palatal approximant,
      , Set.fromList
          [ MINUS_SYLLABIC
          , MINUS_CONSONANTAL
          , PLUS_SONORANT
          , PLUS_CONTINUANT
          , PLUS_VOICE
          , DORSAL
          ])
    , ( Sound "ʍ" -- 028D, vl labial-velar co-articulated approximant,
      , Set.fromList
          [ MINUS_SYLLABIC
          , MINUS_CONSONANTAL
          , PLUS_SONORANT
          , PLUS_CONTINUANT
          , MINUS_VOICE
          , LABIAL
          , DORSAL
          ])
    , ( Sound "w" -- 0077, vd labial-velar co-articulated approximant
      , Set.fromList
          [ MINUS_SYLLABIC
          , MINUS_CONSONANTAL
          , PLUS_SONORANT
          , PLUS_CONTINUANT
          , PLUS_VOICE
          , LABIAL
          , DORSAL
          ])
    ]

vowels :: Map.Map Sound FeatureSet
vowels =
  Map.fromList
    [ ( Sound "i" -- , 0069, close front unrounded,
      , Set.fromList
          [ PLUS_SYLLABIC
          , MINUS_CONSONANTAL
          , PLUS_SONORANT
          , PLUS_CONTINUANT
          , MINUS_ROUND
          , PLUS_HIGH
          , MINUS_LOW
          , MINUS_BACK
          , PLUS_ATR
          ])
    , ( Sound "ɪ" -- , 026A, near-close front unrounded,
      , Set.fromList
          [ PLUS_SYLLABIC
          , MINUS_CONSONANTAL
          , PLUS_SONORANT
          , PLUS_CONTINUANT
          , MINUS_ROUND
          , PLUS_HIGH
          , MINUS_LOW
          , MINUS_BACK
          , MINUS_ATR
          ])
    , ( Sound "ɛ" -- , 025B, open-mid front unrounded,
      , Set.fromList
          [ PLUS_SYLLABIC
          , MINUS_CONSONANTAL
          , PLUS_SONORANT
          , PLUS_CONTINUANT
          , MINUS_ROUND
          , MINUS_HIGH
          , MINUS_LOW
          , MINUS_BACK
          , PLUS_ATR
          ])
    , ( Sound "æ" --, 00E6, near-open front unrounded,
      , Set.fromList
          [ PLUS_SYLLABIC
          , MINUS_CONSONANTAL
          , PLUS_SONORANT
          , PLUS_CONTINUANT
          , MINUS_ROUND
          , MINUS_HIGH
          , PLUS_LOW
          , MINUS_BACK
          , MINUS_ATR
          ])
    , ( Sound "ə" -- , 0259, mid central unrounded,
      , Set.fromList
          [ PLUS_SYLLABIC
          , MINUS_CONSONANTAL
          , PLUS_SONORANT
          , PLUS_CONTINUANT
          , MINUS_ROUND
          , MINUS_HIGH
          , MINUS_LOW
          , MINUS_BACK
          , MINUS_ATR
          ])
    , ( Sound "ʌ" -- , 028C, open-mid back unrounded,
      , Set.fromList
          [ PLUS_SYLLABIC
          , MINUS_CONSONANTAL
          , PLUS_SONORANT
          , PLUS_CONTINUANT
          , MINUS_ROUND
          , MINUS_HIGH
          , MINUS_LOW
          , PLUS_BACK
          , MINUS_ATR
          ])
    , ( Sound "ɑ" -- , 0251, open back unrounded,
      , Set.fromList
          [ PLUS_SYLLABIC
          , MINUS_CONSONANTAL
          , PLUS_SONORANT
          , PLUS_CONTINUANT
          , MINUS_ROUND
          , MINUS_HIGH
          , PLUS_LOW
          , PLUS_BACK
          , MINUS_ATR
          ])
    , ( Sound "u" -- , 0075, close back rounded,
      , Set.fromList
          [ PLUS_SYLLABIC
          , MINUS_CONSONANTAL
          , PLUS_SONORANT
          , PLUS_CONTINUANT
          , PLUS_ROUND
          , PLUS_HIGH
          , MINUS_LOW
          , PLUS_BACK
          , PLUS_ATR
          ])
    , ( Sound "ʊ" -- , 028A, near-close back rounded,
      , Set.fromList
          [ PLUS_SYLLABIC
          , MINUS_CONSONANTAL
          , PLUS_SONORANT
          , PLUS_CONTINUANT
          , PLUS_ROUND
          , PLUS_HIGH
          , MINUS_LOW
          , PLUS_BACK
          , MINUS_ATR
          ])
    , ( Sound "ɔ" -- , 0254, open-mid back rounded,
      , Set.fromList
          [ PLUS_SYLLABIC
          , MINUS_CONSONANTAL
          , PLUS_SONORANT
          , PLUS_CONTINUANT
          , PLUS_ROUND
          , MINUS_HIGH
          , MINUS_LOW
          , PLUS_BACK
          , MINUS_ATR
          ])
    , ( Sound "e͡ɪ" -- , 0065 0361 026A,
      , Set.fromList -- diphthong, closing mid-to-high narrow front  
          [ PLUS_SYLLABIC
          , MINUS_CONSONANTAL
          , PLUS_SONORANT
          , PLUS_CONTINUANT
          , MINUS_ROUND
          , MINUS_HIGH
          , MINUS_LOW
          , MINUS_BACK
          , PLUS_ATR
          , MINUS_WIDE
          ])
    , ( Sound "a͡ɪ" -- , 0061 0361 026A, diphthong closing low-to-high wide,
      , Set.fromList
          [ PLUS_SYLLABIC
          , MINUS_CONSONANTAL
          , PLUS_SONORANT
          , PLUS_CONTINUANT
          , MINUS_ROUND
          , MINUS_HIGH
          , MINUS_LOW
          , MINUS_BACK
          , MINUS_ATR
          , PLUS_WIDE
          ])
    , ( Sound "a͡ʊ" -- , 0061 0361 028A, diphthong closing low-to-high backward front-to-back wide,
      , Set.fromList
          [ PLUS_SYLLABIC
          , MINUS_CONSONANTAL
          , PLUS_SONORANT
          , PLUS_CONTINUANT
          , PLUS_ROUND
          , MINUS_HIGH
          , PLUS_LOW
          , MINUS_BACK
          , MINUS_ATR
          , PLUS_WIDE
          ])
    , ( Sound "o͡ʊ" -- , 006F 0361 028A, diphthong closing mid-to-high narrow back,
      , Set.fromList
          [ PLUS_SYLLABIC
          , MINUS_CONSONANTAL
          , PLUS_SONORANT
          , PLUS_CONTINUANT
          , PLUS_ROUND
          , MINUS_HIGH
          , MINUS_LOW
          , PLUS_BACK
          , PLUS_ATR
          , MINUS_WIDE
          ])
    , ( Sound "ɔ͡ɪ" -- , 0254 0361 026A, diphthong closing mid-to-high forward back-to-front wide,
      , Set.fromList
          [ PLUS_SYLLABIC
          , MINUS_CONSONANTAL
          , PLUS_SONORANT
          , PLUS_CONTINUANT
          , MINUS_ROUND
          , MINUS_HIGH
          , MINUS_LOW
          , PLUS_BACK
          , MINUS_ATR
          , PLUS_WIDE
          ])
    , ( Sound "ɜ˞" -- , 025C 02DE, mid central rhotic stressed,
      , Set.fromList
          [ PLUS_SYLLABIC
          , MINUS_CONSONANTAL
          , PLUS_SONORANT
          , PLUS_CONTINUANT
          , MINUS_ROUND
          , MINUS_HIGH
          , MINUS_LOW
          , MINUS_BACK
          , MINUS_ATR
          , RHOTIC
          , PLUS_STRESSED
          ])
    , ( Sound "ə˞" -- , 0259 02DE, mid central rhotic unstressed,
      , Set.fromList
          [ PLUS_SYLLABIC
          , MINUS_CONSONANTAL
          , PLUS_SONORANT
          , PLUS_CONTINUANT
          , MINUS_ROUND
          , MINUS_HIGH
          , MINUS_LOW
          , MINUS_BACK
          , MINUS_ATR
          , RHOTIC
          , MINUS_STRESSED
          ])
    ]
