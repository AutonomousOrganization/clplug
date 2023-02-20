{-# LANGUAGE
    LambdaCase, 
    DuplicateRecordFields, 
    TypeSynonymInstances, 
    FlexibleInstances
#-} 
module Search (
      Search
    , Way
    , evalTo
    , evalRefs
    , evalChans
    , results'
    , results
    , search
    , hydrate
    , increment
    , chop
    , getNodeInt
    )where 
import Numeric 
import Data.Char
import System.IO 
import Data.Lightning 
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Control.Monad.Reader
import Control.Monad.State
import Control.Concurrent
import Control.Concurrent.Chan
import qualified Data.Sequence as Q
import Data.Sequence (Seq(..),(<|),(|>),(><)) 
import Data.Foldable 
import Route 

type Search b = Reader (Gr () b, Node, Node)  -- from / to
type Way b = Q.Seq b 
type Ref = Q.Seq Int 
type Deref b = (Ref, Way b) 
type Eebe b = Either (Deref b) (Way b)

evalTo :: Channel b => Gr () b -> Node -> Node -> Int -> [Ref]
evalTo = evalBy resLength

evalChans :: Channel b => Gr () b -> Node -> Node -> Int -> [Way b]
evalChans = evalBy results'
evalRefs :: Channel b => Gr () b -> Node -> Node -> Int -> [Ref]
evalRefs = evalBy results
evalBy s g n m l = (`runReader` (g, n, m)) .  (`evalStateT` (Empty, [])) $ s l 

results' :: Channel b => Int -> StateT (Ref, [Way b]) (Search b) [Way b] 
results' x = do  
    (r , c) <- get
    (w, r') <- lift $ search r
    put (increment.chop $ r', w : c) 
    if x > length c 
        then results' x 
        else return c

results :: Channel b => Int -> StateT (Ref, [Ref]) (Search b) [Ref] 
results x = do 
    (r , c) <- get
    (_, r') <- lift $ search r
    put (increment.chop $ r', r' : c) 
    if x > length c 
        then results x 
        else return c


resLength :: Channel b => Int -> StateT (Ref, [Ref]) (Search b) [Ref] 
resLength j = do 
    (r, c) <- get
    (_, r') <- lift $ search r
    put (increment.chop $ r', r':c) 
    if j >= length r' 
        then resLength j
        else return c

search :: Channel b => Ref -> Search b (Way b, Ref)  
search r = (hydrate r) >>= \case
    (Left x) -> do 
        search $ nextr r x
    (Right y) -> do
        (fin (r, y) ) >>= \case  
            Nothing -> search $ increment r
            (Just z) -> lift $ pure z   

fin :: Channel b => (Ref, Way b) -> Search b (Maybe (Way b, Ref)) 
fin (r, w) = do 
    (g, n, v) <- ask 
    oo <- outgoing w 
    let {
        f = dropWhile (not.(== v).fst) oo;
        lo = length oo; 
        la = length f ;
        rr = lo - la 
        }
    case f of 
        [] -> pure Nothing 
        (x:_) -> pure $ Just ( w |> snd x, r |> rr) 

nextr :: Channel b => Ref -> Deref b -> Ref 
nextr r (r', c)  
    | z == Q.length r = extend.increment.chop $ r
    | z == 0 = extendTo (Q.length r + 1) Empty
    | otherwise = extendTo (Q.length r) $ increment $ Q.take z r
    where z = Q.length c

hydrate :: Channel b => Ref -> (Search b) (Eebe b)
hydrate r = evalStateT h (r, Empty) 
    
h :: Channel b => StateT (Deref b) (Search b) (Eebe b)
h = get >>= \case 
    (Empty, c) -> return $ Right c
    dr@(y :<| t, c) -> do 
        (g, n, v) <- lift ask 
        oo <- lift $ outgoing c
        case oo !? y of 
            Nothing     -> pure $ Left dr 
            Just (m, x) -> put (t, c |> x) >> h
 
outgoing :: Channel b => Way b -> Search b [(Node, b)] 
outgoing Empty = do 
    (g, n, v) <- ask 
    pure $ lsuc g n
outgoing (c :|> d) = do 
    (g, n, v) <- ask
    pure $ lsuc g (toNode d) 

increment :: Ref -> Ref
increment Empty = Q.singleton 0
increment (r :|> x) = r |> (x + 1) 
extend :: Ref -> Ref
extend r = r |> 0
chop :: Ref -> Ref
chop Empty = Empty 
chop (r :|> _) = r
extendTo :: Int -> Ref -> Ref 
extendTo x r
    | length r >= x = r 
    | otherwise = extendTo x $ extend r
        
toNode :: Channel c => c -> Node
toNode = getNodeInt.cDest 
getNodeInt s = case readHex.filter isHexDigit $ show s of
    ([]) -> 0
    (x:_)-> fst x

(!?) :: (Foldable t, Eq b, Num b) => t a -> b -> Maybe a
(!?) = foldr voo (const Nothing)
    where 
        voo :: (Eq b, Num b) => a -> (b -> Maybe a) -> b -> Maybe a
        voo x r k 
            | k == 0 = Just x
            | otherwise = r $ k-1 
