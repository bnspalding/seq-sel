module Sounds.GenAm where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Feature
import Sound

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
