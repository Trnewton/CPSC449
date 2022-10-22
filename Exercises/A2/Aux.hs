findCenterVal :: STree a -> a
findCenterVal Leaf = error "Error: Tree is a leaf."
findCenterVal tr = a
    where (Node _ a _) = findCenter tr


findCenter :: STree a -> STree a
findCenter Leaf = Leaf
findCenter tr = findCenter' tr (0,0)
    where
        findCenter' tr@(Node lt _ rt) lr
            | abs (big - sml) <=1    = tr -- Found center
            | big > sml              = findCenter' rt (lr `tp` (sml+1,0)) -- Look down right
            | otherwise              = findCenter' lt (lr `tp` (0,big+1)) -- Look down left
            where
                (sml, big) = (numNodes lt, numNodes rt ) `tp` lr

split :: STree a -> (Int,Int)
split tr = foldavl (\lt _ rt -> (tadd lt + 1, tadd rt + 1)) (0,0) tr
    where
        tadd = uncurry (+)

splits :: STree a -> STree (Int,Int)
splits tr = foldavl splits' Leaf tr
    where
        splits' Leaf _ Leaf                             = (Node Leaf (0, 0) Leaf)
        splits' lt@(Node _ la _) _ Leaf                 = (Node lt (tadd la + 1, 0) Leaf)
        splits' Leaf _ rt@(Node _ ra _)                 = (Node Leaf (0, tadd ra + 1) rt)
        splits' lt@(Node _ la _) _ rt@(Node _ ra _)     = (Node lt (tadd la + 1, tadd ra + 1) rt)

        tadd = uncurry (+)

numBelow :: STree a -> STree Int
numBelow = foldavl numBelow' Leaf
    where
        numBelow' Leaf _ Leaf                           = (Node Leaf 0 Leaf)
        numBelow' Leaf _ rt@(Node _ br _)               = (Node Leaf (br+1) rt)
        numBelow' lt@(Node _ bl _) _ Leaf               = (Node lt (bl+1) Leaf)
        numBelow' lt@(Node _ bl _) _ rt@(Node _ br _)   = (Node lt (bl+2+br) rt)

data STree a
    = Node (STree a) a (STree a)
    | Leaf
  deriving (Show, Eq, Ord, Read)

foldavl :: (b -> a -> b -> b) -> b -> STree a -> b
foldavl _ base Leaf = base
foldavl f base (Node lt a rt) = f (foldavl f base lt) a (foldavl f base rt)


--io = foldavl g (
data M a = M a | N
  deriving Eq

instance Ord a => Ord (M a)
  where
    N <= _ = True
    (M a) <= N = False
    (M a) <= (M b) = a <= b


orderp :: (Ord a) => STree a -> Bool
orderp = snd . (foldavl g (N, False))
  where
    g a b c = k c $ k (M b,True) a
    k (a,b) (c,d) = (max a c, foldr (&&) True [b,d,a>c])