{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE UnicodeSyntax             #-}

module Hkl.PyFAI.Poni
       ( Pose(..)
         -- Poni
       , Poni
       , PoniPath
       , poniP
       , poniToText
         --  PoniEntry
       , PoniEntry
       , poniEntryFlip
       , poniEntryFromList
       , poniEntryRotation
       , poniEntryTranslation
       , poniEntryToList
       , poniEntrySet
       , poniEntryMove
         -- other
       , fromAxisAndAngle
       ) where

import           Control.Applicative               (many, optional, pure, (*>),
                                                    (<$>), (<*), (<*>), (<|>))
import           Data.Attoparsec.Text              (Parser, double, endOfLine,
                                                    isEndOfLine, many1, string,
                                                    takeTill, (<?>))
import           Data.Text                         (Text, append, intercalate,
                                                    pack)
import           Data.Vector.Storable              (Vector, fromList)
import           Numeric.LinearAlgebra             (Matrix, atIndex, fromLists,
                                                    ident, scalar, tr, (<>))
import           Numeric.Units.Dimensional.Prelude (Angle, Length, degree,
                                                    meter, one, radian, (*~),
                                                    (+), (/~), (/~~))
import           Prelude                           hiding ((<>))

import           Hkl.Detector
import           Hkl.MyMatrix
import           Hkl.PyFAI.Detector
import           Hkl.Types

type PoniPath = FilePath

--  Pose

newtype Pose = Pose (MyMatrix Double) deriving (Show)

--  Poni

data PoniEntry = PoniEntry { poniEntryHeader     :: [Text]
                           , poniEntryDetector   :: Maybe SomeDetector --  Detector Name
                           , poniEntryPixelSize1 :: Length Double --  pixels size 1
                           , poniEntryPixelSize2 :: Length Double --  pixels size 1
                           , poniEntryDistance   :: Length Double --  pixels size 2
                           , poniEntryPoni1      :: Length Double --  poni1
                           , poniEntryPoni2      :: Length Double --  poni2
                           , poniEntryRot1       :: Angle Double --  rot1
                           , poniEntryRot2       :: Angle Double --  rot2
                           , poniEntryRot3       :: Angle Double --  rot3
                           , poniEntrySpline     :: Maybe Text --  spline file
                           , poniEntryWavelength :: WaveLength --  wavelength
                           }
               deriving (Show)

type Poni = [PoniEntry]

class ToPoni a where
  toPoni ∷ a → Text

instance ToPoni SomeDetector where
  toPoni (SomeDetector v) = toPyFAI v

instance ToPoni Double where
  toPoni v = pack $ show v

instance ToPoni Text where
  toPoni = id

commentP :: Parser Text
commentP =  "#" *> takeTill isEndOfLine <* endOfLine <?> "commentP"

headerP :: Parser [Text]
headerP = many1 commentP <?> "headerP"

doubleP :: Text -> Parser Double
doubleP key = string key *> double <* endOfLine <?> "doubleP"

lengthP :: Text -> Parser (Length Double)
lengthP key = do
  value <-doubleP key
  pure $ value *~ meter

angleP :: Text -> Parser (Angle Double)
angleP key = do
    value <-doubleP key
    pure $ value *~ radian

detectorP ∷ ToPyFAI a ⇒ a → Parser a
detectorP d = do
  _ ← "Detector: " *> string (toPyFAI d) <* endOfLine
  pure d

aDetectorP ∷ Parser SomeDetector
aDetectorP = (SomeDetector <$> detectorP Xpad32) <|> (SomeDetector <$> detectorP ImXpadS140)

poniEntryP :: Parser PoniEntry
poniEntryP = PoniEntry
        <$> headerP
        <*> optional aDetectorP
        <*> lengthP "PixelSize1: "
        <*> lengthP "PixelSize2: "
        <*> lengthP "Distance: "
        <*> lengthP "Poni1: "
        <*> lengthP "Poni2: "
        <*> angleP "Rot1: "
        <*> angleP "Rot2: "
        <*> angleP "Rot3: "
        <*> optional ("SplineFile: " *> takeTill isEndOfLine <* endOfLine)
        <*> lengthP "Wavelength: "
        <?> "poniEntryP"

poniP :: Parser Poni
poniP = many poniEntryP

poniToText :: Poni -> Text
poniToText p = Data.Text.intercalate (Data.Text.pack "\n") (map poniEntryToText p)

poniEntryToText :: PoniEntry -> Text
poniEntryToText p = intercalate (Data.Text.pack "\n") $
                    map (Data.Text.append "#") (poniEntryHeader p)
                    ++ maybe [] (poniLine "Detector: ") (poniEntryDetector p)
                    ++ poniLine "PixelSize1: " (poniEntryPixelSize1 p /~ meter)
                    ++ poniLine "PixelSize2: " (poniEntryPixelSize2 p /~ meter)
                    ++ poniLine "Distance: " (poniEntryDistance p /~ meter)
                    ++ poniLine "Poni1: " (poniEntryPoni1 p /~ meter)
                    ++ poniLine "Poni2: " (poniEntryPoni2 p /~ meter)
                    ++ poniLine "Rot1: " (poniEntryRot1 p /~ radian)
                    ++ poniLine "Rot2: " (poniEntryRot2 p /~ radian)
                    ++ poniLine "Rot3: " (poniEntryRot3 p /~ radian)
                    ++ maybe [] (poniLine "SplineFile: ") (poniEntrySpline p)
                    ++ poniLine "Wavelength: " (poniEntryWavelength p /~ meter)
  where
    poniLine :: ToPoni a ⇒ String → a → [Text]
    poniLine key v = [Data.Text.append (Data.Text.pack key) (toPoni v)]

crossprod :: Vector Double -> Matrix Double
crossprod axis = fromLists [[ 0, -z,  y],
                            [ z,  0, -x],
                            [-y,  x,  0]]
    where
      x = axis `atIndex` 0
      y = axis `atIndex` 1
      z = axis `atIndex` 2

fromAxisAndAngle :: Vector Double -> Angle Double -> Matrix Double
fromAxisAndAngle axis angle = ident 3 Prelude.+ s * q Prelude.+ c * (q <> q)
    where
      c = scalar (1 - cos (angle /~ one))
      s = scalar (sin (angle /~ one))
      q = crossprod axis

poniEntryFlip :: PoniEntry -> PoniEntry
poniEntryFlip p = p { poniEntryRot3 = new_rot3 }
  where
    rot3 = poniEntryRot3 p
    new_rot3 = rot3 Numeric.Units.Dimensional.Prelude.+ 180 *~ degree

poniEntryRotation :: PoniEntry ->  Matrix Double -- TODO MyMatrix PyFAIB
poniEntryRotation e = Prelude.foldl (<>) (ident 3) rotations
    where
      rot1 = poniEntryRot1 e
      rot2 = poniEntryRot2 e
      rot3 = poniEntryRot3 e
      rotations = Prelude.map (uncurry fromAxisAndAngle)
                  [ (fromList [0, 0, 1], rot3)
                  , (fromList [0, 1, 0], rot2)
                  , (fromList [1, 0, 0], rot1)]

poniEntryTranslation :: PoniEntry -> Vector Double
poniEntryTranslation e = fromList ( [ poniEntryPoni1 e
                                    , poniEntryPoni2 e
                                    , poniEntryDistance e
                                    ] /~~ meter )

poniEntryMove :: MyMatrix Double -> MyMatrix Double -> PoniEntry -> PoniEntry
poniEntryMove mym1 mym2 e = e { poniEntryRot1 = new_rot1
                              , poniEntryRot2 = new_rot2
                              , poniEntryRot3 = new_rot3
                              }
  where
    rot1 = poniEntryRot1 e
    rot2 = poniEntryRot2 e
    rot3 = poniEntryRot3 e
    rotations = Prelude.map (uncurry fromAxisAndAngle)
                [ (Data.Vector.Storable.fromList [0, 0, 1], rot3)
                , (Data.Vector.Storable.fromList [0, 1, 0], rot2)
                , (Data.Vector.Storable.fromList [1, 0, 0], rot1)]
    -- M1 . R0 = R1
    r1 = Prelude.foldl (<>) (ident 3) rotations -- pyFAIB
    -- M2 . R0 = R2
    -- R2 = M2 . M1.T . R1
    r2 = Prelude.foldl (<>) m2 [tr m1, r1]
    (new_rot1, new_rot2, new_rot3) = toEulerians r2

    (MyMatrix _ m1) = changeBase mym1 PyFAIB
    (MyMatrix _ m2) = changeBase mym2 PyFAIB

poniEntrySet ∷ Length Double --  distance
             → Length Double --  poni1
             → Length Double --  poni2
             → Angle Double --  rot1
             → Angle Double --  rot2
             → Angle Double --  rot3
             → PoniEntry
             → PoniEntry
poniEntrySet d p1 p2 r1 r2 r3 p =
  p { poniEntryDistance = d
    , poniEntryPoni1 = p1
    , poniEntryPoni2 = p2
    , poniEntryRot1 = r1
    , poniEntryRot2 = r2
    , poniEntryRot3 = r3
    }

poniEntryFromList :: PoniEntry -> [Double] -> PoniEntry
poniEntryFromList p [rot1, rot2, rot3, poni1, poni2, d] =
  p { poniEntryDistance = d *~ meter
    , poniEntryPoni1 = poni1 *~ meter
    , poniEntryPoni2 = poni2 *~ meter
    , poniEntryRot1 = rot1 *~ radian
    , poniEntryRot2 = rot2 *~ radian
    , poniEntryRot3 = rot3 *~ radian
    }
poniEntryFromList _ _ = error "Can not convert to a PoniEntry"

poniEntryToList :: PoniEntry -> [Double]
poniEntryToList p = [ poniEntryRot1 p /~ radian
                    , poniEntryRot2 p /~ radian
                    , poniEntryRot3 p /~ radian
                    , poniEntryPoni1 p /~ meter
                    , poniEntryPoni2 p /~ meter
                    , poniEntryDistance p /~ meter
                    ]
