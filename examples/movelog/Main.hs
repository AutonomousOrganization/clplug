{-# LANGUAGE 
      OverloadedStrings
    , RecordWildCards
    , DuplicateRecordFields
    , ViewPatterns
    , NamedFieldPuns
    , LambdaCase 
    , FlexibleContexts
    , DeriveAnyClass
#-}

module Main where

import Control.Plugin
import Data.Lightning 
import Control.Monad.Reader
import Control.Monad.State
import Control.Exception
import Data.Aeson 
import Data.Aeson.Types
import Data.Maybe 
import Data.Text (Text) 
import qualified Data.Text as T 
import qualified Data.Text.IO as T 
import System.Directory 
import Fmt 
import Data.Text.Format.Numbers
import Data.Time.LocalTime
import Data.Time.Format

main :: IO ()
main = plugin manifest start app 

manifest = object [
      "dynamic" .= True
    , "subscriptions" .= (["coin_movement"] :: [Text] ) 
    , "options" .= ([
          Option "logfile" "string" "" "file path for logs" False
        ])
    , "rpcmethods" .= ([
          RpcMethod "catchcheck" "" "xd" Nothing False
        ]) 
    ] 

start :: PluginInit Msat
start _ = pure 0   

data MoveError = MissingFile deriving (Show, Exception) 

app :: PluginApp Msat 
app (Nothing, "coin_movement", fromJSON -> Success (CoinMovement{..})) = do 
    (Just filepath) <- asks getFile 
    fileExists <- liftIO $ doesFileExist filepath
    if not fileExists then throw MissingFile else pure () 
    case tags of 
        ["routed"] -> if hasVal debit_msat && hasVal fees_msat then do 
            total <- state (accumulate now) 
            liftIO $ movelog filepath total now 
            else pure ()  
            where 
                now = fromJust fees_msat
                hasVal a = isJust a && ((> 0).fromJust) a
                accumulate n t = (x,x) where x = n + t
        otherMove -> liftIO $ T.appendFile filepath $ fmtLn . build $ otherMove
app (Just i, _, _) = release i
app _ = pure ()  

movelog :: FilePath -> Msat -> Msat -> IO () 
movelog path tots now = do
    timenow <- liftIO getTime
    liftIO . (T.appendFile path) . fmtLn $ mlog timenow
        where 
            mlog t = (build.(prettyI (Just ',')).(`div` 1000) $ tots) 
                +| " ~~ " 
                +| build t 
                +| " +"
                +| build now

            getTime = do 
                zone <- getZonedTime 
                pure $ formatTime defaultTimeLocale "%H:%M" zone 
    
getFile :: PlugInfo -> Maybe FilePath 
getFile (_, Init opts _) =  parseMaybe (.: "logfile") opts 
