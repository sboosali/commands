module Commands.Plugins.Spiros.Rank where 
import Commands.Plugins.Spiros.Extra.Types 


type Ranking a = a -> Int

-- lawless, domain-specific, 'Ord'-like typeclass.
-- used by @data Apply@, permitting "ranking a function" by ranking its arguments (before application). 
-- can be derived, avoiding boilerplate. 
class Rankable a where          -- TODO a ranking that's relative, not absolute.
 rank :: Ranking a
 rank _ = defaultRank

instance Rankable Int where rank = const defaultRank
instance Rankable Ordinal where rank = const defaultRank 
instance (Rankable a) => Rankable (Maybe a) where rank = rankMaybe
instance (Rankable a, Rankable b) => Rankable (Either a b) where rank = rankLeftBiased

defaultRank :: Int
defaultRank = 100               -- TODO 

highRank :: Int
highRank = 1000

defaultRankMultiplier :: Int
defaultRankMultiplier = 1000

rankAtLeast :: Rankable a => Int -> a -> Int
rankAtLeast i a = min i (rank a) 

-- instance Rankable (Either Shell Phrase) where rank = rankLeftBiased
-- instance Rankable (Either Ordinal Phrase) where rank = rankLeftBiased

rankMaybe :: (Rankable a) => Ranking (Maybe a)
rankMaybe = maybe defaultRank rank

rankLeftBiased :: (Rankable a, Rankable b) => Ranking (Either a b)
rankLeftBiased = either ((*defaultRankMultiplier) . rank) rank -- watch out, the multiplied rank pollutes any parents above it 
-- rankLeftBiased = either rank ((`div` defaultRankMultiplier) . rank)

