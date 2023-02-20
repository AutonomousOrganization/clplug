{-# LANGUAGE 
      DuplicateRecordFields
    , OverloadedStrings
    , DeriveGeneric #-} 

module Route where 

import Data.Lightning 
import Numeric (readHex) 
import Data.Aeson
import GHC.Generics
import Data.Text (Text) 
import Control.Applicative 

data Route = Route {
      __id :: Text 
    , channel :: Text 
    , direction :: Int
    , __amount_msat :: Msat 
    , __delay :: Int 
    , style :: Text
    } deriving (Show, Generic, Eq) 
instance ToJSON Route where
    toJSON v = genericToJSON defaultOptions{fieldLabelModifier = dropWhile (=='_')} v 

class Channel c where
      basefee :: c -> Msat
      ppmrate :: c -> Int 
      cDelay  :: c -> Int
      cDest   :: c -> Text
      cSource :: c -> Text
      shortid :: c -> Text 
    
createRoute :: (Channel c) => Msat -> [c] -> [Route]
createRoute a c = foldr (addHop a) [] $ pairUp c
    where
        pairUp :: [c] -> [(c,c)]
        pairUp [] = []
        pairUp (a:[]) = [(a,a)]
        pairUp (a:b) = (a,head b) : pairUp b

        -- addHop :: Msat -> (Channel, Channel) -> [Route] -> [Route]
        addHop a (cp, c)  r = Route
            (cDest cp)
            (shortid cp)
            (liftA2 getDirect cSource cDest cp)
            (getAmount a c r)
            (getDelay c r)
            "tlv"
            : r

        -- getDirect :: String -> String -> Int
        getDirect a b = if readHex (show a) < readHex (show b) then 0 else 1

        -- getDelay :: Channel -> [Route] -> Int
        getDelay e [] = 9
        getDelay e (r:_) = __delay r + cDelay e
        
        getAmount :: (Channel c) => Msat -> c -> [Route] -> Msat
        getAmount a e [] = a
        getAmount a e r =
            let mil = 1000000 :: Integer
                b = basefee e
                p = ppmrate e
                num = (mil * toInteger nextAmount * toInteger p)
                denum = mil*mil
                ppmfee  = fromInteger $ div num denum -- inaccurate?>
                nextAmount = maximum $ map __amount_msat r
            in sum [ nextAmount, b, ppmfee ]


