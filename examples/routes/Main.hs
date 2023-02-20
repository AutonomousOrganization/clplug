{-# LANGUAGE 
      DuplicateRecordFields
    , LambdaCase
    , DeriveGeneric
    , OverloadedStrings
    , BangPatterns
    , ViewPatterns
#-}
module Main where 

import Numeric 
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Char 
import Control.Plugin
import Data.Lightning 
import Control.Client
import Data.Aeson
import Data.Aeson.Lens
import Control.Lens hiding ((.=))
import Control.Monad.IO.Class
import GHC.Generics  
import Data.Text (Text)
import Data.Lightning 
import Data.Lightning.Generic 
import Control.Client 
import Control.Conduit
import Control.Monad.State
import Control.Monad.Reader 
import Control.Applicative (liftA2) 
import Data.List 
import Data.Foldable 
import Route 
import Search 

type Gra = Gr () MyChan

main = plugin manifest start app

manifest = object [
      "dynamic" .= True
    , "options" .= ([] :: [Option])
    , "rpcmethods" .= [
         RpcMethod "route" "[n, m, a, l]" "l routes from n to m of a" Nothing False 
       , RpcMethod "network" "" "show metrics of loaded graph" Nothing False 
       ]
    ]

start :: PluginInit Gra
start (h, Init o c) = do
    Just (Res xd _) <- liftIO $ lightningCli h (Command "listnodes" nfilt noparams)
    Just (Res yd _) <- liftIO $ lightningCli h (Command "listchannels" cfilt noparams)
    case (fromJSON xd, fromJSON yd) :: (Result MyNodes, Result MyChans) of
        (Success nx, Success cx) -> pure $ mkGraph 
            (map toLNode (_nodes nx)) (map toLEdge' (channels cx))     
        _ -> pure empty
        where toLNode ni = ((getNodeInt.nodeid) ni , ())
              toLEdge' c = (
                      (getNodeInt.cSource) c
                    , (getNodeInt.cDest) c
                    , c
                    )

app (Just i, "route", v) =
    let n = getNodeInt <$> v ^? nth 0 
        m = getNodeInt <$> v ^? nth 1  
        a = maybe 100000000 fromInteger $ v ^? nth 2 . _Integer
        l = maybe 1 fromInteger $ v ^? nth 3 . _Integer
    in do
        g <- get
        case valid g n m of
            Just (n', m') -> do 
                r <- pure . map (createRoute a . toList) $ evalChans g n' m' l
                respond (object ["routes" .= r ]) i 
            _ -> respond (object ["invalid nodid" .= True]) i 
            where 

app (Just i, "network", _) = do 
    g <- get
    respond (object [
          "nodes" .= order g
        , "edges" .= size g
        , "capacity" .= capacity g 0
        ]) i 
        where capacity g t 
                  | isEmpty g = t  
                  | True = case matchAny g of 
                      (n, g') -> capacity g' $ t + (sum 
                          . map _amount_msat
                          . map snd  
                          . lsuc' $ n )

app (Just i, _, _) = release i
app _ = pure () 


valid g n m = case (n , m) of 
    (Just n', Just m') -> if gelem n' g && gelem m' g
                          then (Just (n', m'))
                          else Nothing   
    _ -> Nothing
noparams = object [] 
nfilt = object ["nodes" .= [nField]] 
nField = object ["nodeid" .= True] 

data MyNodes = M {
    _nodes :: [MyNode]  
    } deriving Generic 
instance FromJSON MyNodes where
    parseJSON = defaultParse
data MyNode = MyNode {
    nodeid :: Text
    } deriving Generic
instance FromJSON MyNode 

cfilt = object ["channels" .= [object [
      "source" .= True
    , "destination" .= True
    , "short_channel_id" .= True
    , "delay" .= True
    , "base_fee_millisatoshi" .= True       
    , "fee_per_millionth" .= True 
    , "amount_msat" .= True       
    ]]]

data MyChans = CC {
      channels :: [MyChan] 
    } deriving Generic
instance FromJSON MyChans
data MyChan = MyChan {
      _source :: Text
    , _destination :: Text
    , _short_channel_id :: Text
    , _delay :: Int
    , _base_fee_millisatoshi :: Msat
    , _fee_per_millionth :: Int 
    , _amount_msat :: Msat 
    } deriving Generic
instance FromJSON MyChan where 
    parseJSON = defaultParse
-- instance ToJSON MyChan
instance Channel MyChan where -- ? redundant?
    basefee = _base_fee_millisatoshi
    ppmrate = _fee_per_millionth
    cDelay = _delay 
    cDest = _destination
    cSource = _source
    shortid = _short_channel_id

