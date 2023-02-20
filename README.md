
## Core Lightning Plug

Extend [lightningd](https://lightning.readthedocs.io/PLUGINS.html).

The `PluginMonad` contains
- `ask` (Handle,Init) handle to lightning-rpc
- `get/put` polymorphic state
- `yield` to respond

You define
- `plugin :: Manifest -> PluginInit -> PluginApp -> IO ()`
- `Manifest :: Value` configuration
- `PluginInit :: PlugInfo -> IO a` return starting state
- `PluginApp ::  (Maybe Id, Method, Params) -> PluginMonad`

Examples for (o)perators & (d)evelopers 
- **movelog**
    - o - specify logfile= see fees earned and other coin movements
    - d - wherein a notification is subscribed, an option is added, and the state monad is used
- **wallet** 
    - o - show available totals and channel balances: `lightning-cli wallet`
    - d - wherein a new rpc method is created
- **routes** 
    - o - generate routes from bfs: `lightning-cli route` 
    - d - wherein fgl (functional graph library) is loaded and several rpc parameters are used
    
In progress: 
- **tally** tallycoin connect
- **balance** 
- **deploy** 
- **lud4** 
- **hold**
- 

Library exports 
- `Control.Plugin`
- `Control.Client`
- `Data.Lightning`

Useful areas of research:
- what fee set
- what route use
- when rebalance
- why channel close
- where new channel
- why no donate

##### nodeid: 0337694505123a12a8fadd95523dcc235898ad3b80a06e4a63ca26fed68dd0d17c

##### bitcoin addr: bc1q5xx9mathvsl0unfwa3jlph379n46vu9cletshr

lightning only scales bitcoin if people run nodes

- talldo 

```haskell  
{-# LANGUAGE 
      OverloadedStrings 
    , FlexibleContexts 
    , ViewPatterns
    , RecordWildCards
#-} 

module Main (main) where

-- from clplug
import Data.Lightning 
import Control.Plugin  
import Control.Conduit
--

import Data.Conduit 
import Data.Aeson
import Data.Text

main = plugin manifest appState app

manifest :: Value 
manifest = object [
      "dynamic" .= True
    , "subscriptions" .= (["channel_opened"] :: [Text] ) 
    , "options" .= ([]::[Option])
    , "rpcmethods" .= ([]) 
    , "hooks" .= ([]::[Hook])
    , "featurebits" .= object [ ]
    , "notifications" .= ([]::[Notification])
    ] 

app :: PluginApp () 
app (Nothing, "channel_opened", fromJSON -> Success (ChannelOpened {..})) = do
    doublespend funding_txid 
    where doublespend _ = pure ()

appState = pure () 
```      

