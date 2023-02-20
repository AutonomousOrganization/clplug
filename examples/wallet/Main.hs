{-# LANGUAGE
      OverloadedStrings 
    , DeriveGeneric 
    , DuplicateRecordFields
    , ViewPatterns #-}

module Main where 

import Data.Lightning 
import Data.Lightning.Generic 
import Control.Plugin 
import Control.Client 
import Control.Conduit 
import Data.Aeson
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.IO.Class 
import GHC.Generics 
import Data.Text (Text)
import qualified Data.Text as T 
import Fmt 
import Data.Text.Format.Numbers 
import Data.List 
import Data.Maybe

main = plugin manifest start app 

manifest = object [
      "dynamic" .= True
    , "options" .= ([] :: [Option])
    , "rpcmethods" .= [
          RpcMethod "wallet" "" "print summary info" Nothing False 
       ]
    ]

start :: PluginInit ()
start _ = pure () 

app :: PluginApp () 
app (Just i, "wallet", _) = do 
    (h, _) <- ask 
    Just (Res xd _) <- liftIO $ lightningCli h (Command "listfunds" fundFilter fundParams) 
    case fromJSON xd :: Result MyFundy of
        (Success x) -> respond (summarizeFunds x) i  
        _ -> release i 

summarizeFunds :: MyFundy -> Value
summarizeFunds myf = object [
      "withdraw" .= (prettyI (Just ',') . (`div` 1000) . outSum . outputs $ myf)
    , "pay" .= (prettyI (Just ',') (payable $ channels myf))
    , "invoice" .= (prettyI (Just ',') $ recable (channels myf))
    , "xchan" .= (map showChan $ sort $ channels myf)
    ]
    where outSum :: [Outy] -> Msat 
          outSum = sum . map __amount_msat . filter ((=="confirmed") . __status) 
          payable = sum . map our_amount_msat . filter ((=="CHANNELD_NORMAL") . _state)
          recable = sum . map recable' . filter ((=="CHANNELD_NORMAL")._state) 
          recable' a = _amount_msat a - our_amount_msat a
          showChan :: Chany -> Text
          showChan (Chany a o s n i) = "" 
              +| (build $ T.justifyLeft 13 ' ' <$> i ) 
              +| " |" 
              +| (build $ evalState xd (o, ""))
              +| "#"
              +| (build $ evalState xd (a-o, ""))   
              +| "| " 
              +| (build $ T.take 6 n)
              +| " "
              +| if s /= "CHANNELD_NORMAL" then build s else ""  
  
          xd :: State (Msat, Text) Text  
          xd = do 
              (m, t) <- get 
              put (m - 1000000000, T.append t "-" ) 
              if m > 0 && T.length t < 11 then xd else pure t

fundFilter = object [
      "outputs" .= [outputFields]
    , "channels" .= [chanFields]
    ]
outputFields = object [
      "amount_msat" .= True
    , "status" .= True     
    ]
chanFields = object [
      "amount_msat" .= True
    , "our_amount_msat" .= True
    , "state" .= True
    , "peer_id" .= True
    , "short_channel_id" .= True
    ]

data MyFundy = MyFundy {
      outputs :: [Outy]
    , channels :: [Chany] 
    } deriving Generic 
instance FromJSON MyFundy

data Outy = Outy {
      __amount_msat :: Msat 
    , __status :: Text 
    } deriving Generic 
instance FromJSON Outy where 
    parseJSON = defaultParse

data Chany = Chany {
      _amount_msat :: Msat
    , our_amount_msat :: Msat 
    , _state :: Text
    , peer_id :: Text 
    , _short_channel_id :: Maybe Text 
    } deriving (Eq, Generic) 
instance FromJSON Chany where 
    parseJSON = defaultParse
instance Ord Chany where 
    compare x y = compare (_short_channel_id x) (_short_channel_id y)

    
fundParams = object [ ] 
