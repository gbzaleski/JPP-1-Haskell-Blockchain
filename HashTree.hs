-- Author: Grzegorz B. Zaleski (418494)
module HashTree where
import Hashable32
import Utils

-- Constants from task description
indSize :: Int
indSize = 2 -- Indentation size for displaying levels of nodes in HashTree

data Tree a = Leaf Hash a | Twig Hash (Tree a) | Node Hash (Tree a) (Tree a)

drawTree :: Show a => Tree a -> String
drawTree t = drop 1 $ drawTreeAux t 0 ++ "\n" where
    drawTreeAux (Leaf h val) depth = '\n' : replicate depth ' ' ++ showHash h ++ ' ' : show val
    drawTreeAux (Twig h t) depth = '\n' : replicate depth ' ' ++ showHash h ++ " +" ++ drawTreeAux t (depth + indSize)
    drawTreeAux (Node h tl tr) depth = '\n' : replicate depth ' ' ++ showHash h ++ " -" ++ drawTreeAux tl (depth + indSize) ++ drawTreeAux tr (depth + indSize)


treeHash :: Tree a -> Hash
treeHash (Leaf h _) = h
treeHash (Twig h _) = h
treeHash (Node h _ _) = h

leaf :: Hashable a => a -> Tree a
leaf val = Leaf (hash val) val

twig :: Hashable a => Tree a -> Tree a
twig t = let h = treeHash t in Twig (hash (h, h)) t

node :: Hashable a =>  Tree a -> Tree a -> Tree a
node tl tr = Node (hash (treeHash tl, treeHash tr)) tl tr


buildTree :: Hashable a => [a] -> Tree a
buildTree elems = let (len, height) = treeParams leavesLen (0, 0) in construct len leavesLen elems height where

    leavesLen = length elems

    treeParams :: Int -> (Int, Int) -> (Int, Int)
    treeParams 1 (len, height) = (len + 1, height)
    treeParams n (len, height) = treeParams ((n+1) `div` 2) (len + n, height + 1)

    construct :: Hashable a => Int -> Int -> [a] -> Int -> Tree a
    construct len leavesLen leavesValues height
        | height == 0 = Leaf (hash $ head leavesValues) $ head leavesValues
        | len <= 2^height  = Twig headHash leftTree
        | otherwise = Node headHash leftTree rightTree where

            leftLen = min (len - 1) (2^height -1)
            leftLeavesLen = min leavesLen 2^(height -1)
            leftLeaves = take leftLeavesLen leavesValues

            rightLen = len - 2^height
            rightLeavesLen = leavesLen - 2^(height -1)
            rightLeaves = drop leftLeavesLen leavesValues

            newHeight = height - 1

            leftTree = construct leftLen leavesLen leavesValues newHeight
            rightTree = construct rightLen rightLeavesLen rightLeaves newHeight
            headHash = if len <= 2^height
                then hash (treeHash leftTree, treeHash leftTree)
                else hash (treeHash leftTree, treeHash rightTree)


type MerklePath = [Either Hash Hash]
showMerklePath :: MerklePath -> String
showMerklePath = concatMap displayEitherHash where
    displayEitherHash (Left h) = '<' : showHash h
    displayEitherHash (Right h) = '>' : showHash h

data MerkleProof a = MerkleProof a MerklePath
instance Show a => Show (MerkleProof a) where
    showsPrec d (MerkleProof val path) = showParen (d > app_prec) $ showString $ "MerkleProof " ++ showsPrec (app_prec + 1) val ""  ++ ' ' : showMerklePath path where 
        app_prec = 10

buildProof :: Hashable a => a -> Tree a -> Maybe (MerkleProof a)
buildProof elem (Leaf h val) = if hash val == hash elem then Just (MerkleProof elem []) else Nothing
buildProof elem t = let result = buildProofAux elem t [] in if null result then Nothing else Just (MerkleProof elem $ reverse result) where
    buildProofAux :: Hashable a => a -> Tree a -> MerklePath -> MerklePath
    buildProofAux elem (Leaf h val) path = if hash val == hash elem then path else []
    buildProofAux elem (Twig h t) path = buildProofAux elem t (Left (treeHash t):path)
    buildProofAux elem (Node h tl tr) path = if null leftpath then rightpath else leftpath where
        leftpath = buildProofAux elem tl (Left (treeHash tr):path)
        rightpath = buildProofAux elem tr (Right (treeHash tl):path)


merklePaths :: Hashable a => a -> Tree a -> [MerklePath]
merklePaths elem t = merklePathsAux elem t [] where
    merklePathsAux :: Hashable a => a -> Tree a -> MerklePath -> [MerklePath]
    merklePathsAux elem (Leaf h val) path = [reverse path | hash val == hash elem]
    merklePathsAux elem (Twig h t) path = merklePathsAux elem t (Left (treeHash t):path)
    merklePathsAux elem (Node h tl tr) path = leftpath ++ rightpath where
        leftpath = merklePathsAux elem tl (Left (treeHash tr):path)
        rightpath = merklePathsAux elem tr (Right (treeHash tl):path)


verifyProof :: Hashable a => Hash -> MerkleProof a -> Bool
verifyProof h (MerkleProof elem path) = foldr auxHash (hash elem) path == h where
    auxHash :: Either Hash Hash -> Hash -> Hash
    auxHash (Left v) h = hash (h, v)
    auxHash (Right v) h = hash (v, h)
