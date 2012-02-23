{-# LANGUAGE BangPatterns #-}

-- 1 second == 10 / 864 ebeats

module Data.Time.Ebeats
  ( Ebeats(..)
  , EbeatsTime(..)
  , getEbeatsTime
  , getEbeats
  , toEbeatsTime
  , toEbeats
  , ebeatsToSeconds
  , secondsToEbeats
  -- , picosecondsToEbeats
  , diffEbeatsTime
  , addEbeatsTime
  )
  where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

import  Data.Fixed          (Fixed, E2, divMod')
import  Data.Time           (UTCTime(..), getCurrentTime)
import  Data.Time.LocalTime (TimeOfDay(..), timeToTimeOfDay)
import  Data.Time.Calendar  (toGregorian, Day(ModifiedJulianDay), toModifiedJulianDay)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

newtype Ebeats = Ebeats { ebeatsTimeValue :: Fixed E2 }
  deriving (Eq, Ord)

data EbeatsTime = EbeatsTime { days   :: Integer      -- ^ Modified Julian Day, but not typed as such so that we can define a different "show"
                             , ebeats :: Ebeats }
  deriving (Eq, Ord)

instance Show Ebeats where
  show = ('@':) . show . ebeatsTimeValue

instance Show EbeatsTime where
  show ebeatsTime = (show y) ++ "-" ++ (show m) ++ "m-" ++ (show d) ++ "d " ++ (show $ ebeats ebeatsTime) where
    (y,m,d) = toGregorian $ ModifiedJulianDay $ days ebeatsTime

getEbeats :: IO Ebeats
getEbeats = toEbeats `fmap` getCurrentTime

-- Can you spot the difference in coding styles?:
getEbeatsTime :: IO EbeatsTime
getEbeatsTime = do ebeats <- getEbeats
                   now <- getCurrentTime
                   let day = toModifiedJulianDay $ utctDay now
                   return $ EbeatsTime day ebeats


toEbeats :: UTCTime -> Ebeats
toEbeats utct =
    let TimeOfDay h m s = timeToTimeOfDay $ utctDayTime utct
        !ebeats         = realToFrac $ s * 5/432 + realToFrac m * 25/36 + realToFrac h * 125/3
    in Ebeats ebeats

toEbeatsTime :: UTCTime -> EbeatsTime
toEbeatsTime utct =
    let TimeOfDay h m s = timeToTimeOfDay $ utctDayTime utct
        !ebeats         = realToFrac $ s * 5/432 + realToFrac m * 25/36 + realToFrac h * 125/3
        day             = toModifiedJulianDay $ utctDay utct
    in EbeatsTime day (Ebeats ebeats)

diffEbeatsTime :: EbeatsTime
               -> EbeatsTime
               -> Ebeats -- ^ Note that Ebeats is being used to measure the difference between "EbeatsTime"s.
-- ^ diffEbeatsTime a b = a - b   -- Like diffUTCTime.
diffEbeatsTime (EbeatsTime day0 (Ebeats ebeats0)) (EbeatsTime day1 (Ebeats ebeats1))
               =  let total0 = (fromIntegral $ day0 * 1000 :: Fixed E2) + ebeats0
                      total1 = (fromIntegral $ day1 * 1000 :: Fixed E2) + ebeats1
                  in Ebeats (total0 - total1)

addEbeatsTime                                      :: Ebeats -- ^ Note that Ebeats is being used as the measure between "EbeatsTime"s.
                                                   -> EbeatsTime
                                                   -> EbeatsTime
-- ^ addEbeatsTime a b = a + b -- Like addUTCTime.
addEbeatsTime (Ebeats a) (EbeatsTime d (Ebeats e)) =  let (newDays, newEbeats) = divMod' (a + e) 1000
                                                      in EbeatsTime (d + newDays) (Ebeats newEbeats)


instance Num Ebeats where
    (Ebeats a) + (Ebeats b) =  Ebeats (a + b)
    (Ebeats a) - (Ebeats b) =  Ebeats (a - b)
    (Ebeats a) * (Ebeats b) =  Ebeats (a * b) -- No real significance. Just for typeclass-completeness.
    negate (Ebeats a)       =  Ebeats (0 - a)
    abs (Ebeats a)          =  Ebeats (abs a)
    signum (Ebeats a)       =  Ebeats (signum a) -- Kind of nonsensical return value
    fromInteger an_int      =  Ebeats (fromInteger an_int)

-- number of seconds in a day: 24 * 60 * 60 == 86400
ebeatsToSeconds            :: Ebeats
                           -> Fixed E2 -- ^ Number of seconds
ebeatsToSeconds (Ebeats a) =  a * (86400 / 1000)


secondsToEbeats      :: Fixed E2 -- ^ Number of seconds
                     -> Ebeats
secondsToEbeats secs =  Ebeats $ secs * (1000 / 86400)
-- with loss of precision, it's always 1 sec == 0.01 ebeats, huh?
