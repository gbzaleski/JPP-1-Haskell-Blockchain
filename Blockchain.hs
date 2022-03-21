-- Author: Grzegorz B. Zaleski (418494)
module Blockchain where
import Control.Monad
import Data.Word
import Data.Bits
import Hashable32
import HashTree
import PPrint
import Data.Maybe(fromJust)

type Address = Hash
type Amount = Word32

-- Constants from task description
difficulty :: Integer
difficulty = 5
blockReward :: Amount
blockReward = 50 * coin
coin :: Amount
coin = 1000

data Transaction = Tx {
  txFrom :: Address, -- sender
  txTo :: Address, -- receiver
  txAmount :: Amount -- amount to be delivered
  } deriving Show

instance Hashable Transaction where
  hash (Tx f t a) = hash [hash t, hash f, hash a]

data Block = Block {
  blockHdr :: BlockHeader,
  blockTxs :: [Transaction]
  }

instance Show Block where
  show (Block hdr txs) = unlines (show hdr : map show txs)

instance Hashable Block where
  hash = hash . blockHdr

data BlockHeader = BlockHeader {
  parent :: Hash, -- previous block's hash
  coinbase :: Transaction, -- reward transaction (from coinbaseTx) for mining block
  txroot :: Hash, -- root of the Merkle tree for transactions
  nonce :: Hash -- proof of work value (matching hash part)
  } deriving Show

instance Hashable BlockHeader where
  hash (BlockHeader p c r n) = hash [p, hash c, r, n]

coinbaseTx :: Hash -> Transaction
coinbaseTx miner = Tx {txFrom = 0, txTo = miner, txAmount = blockReward}

-- Hash validation with bitwise operations
difficultyBitsMargin = 2 ^ difficulty - 1
validNonce :: BlockHeader -> Bool
validNonce b = hash b .&. difficultyBitsMargin == 0

-- Generator for nonce values - implemented the most obvious one which iterates over natural numbers.
type Nonce = Word32
genNextNonce :: Nonce -> Nonce
genNextNonce currentNonce = currentNonce + 1

type Miner = Address
mineBlock :: Miner -> Hash -> [Transaction] -> Block
mineBlock miner parent txList = 
  let firstHeader = BlockHeader { parent = parent, coinbase = coinbaseTx miner, txroot = treeHash $ buildTree $ coinbaseTx miner:txList, nonce = 0}
  in mineBlockAux miner parent txList firstHeader (genNextNonce 0) where

  mineBlockAux :: Miner -> Hash -> [Transaction] -> BlockHeader -> Nonce -> Block
  mineBlockAux miner parent txList currentHeader nextNonce = if validNonce currentHeader
    then Block {blockHdr = currentHeader, blockTxs = txList}
    else mineBlockAux miner parent txList (currentHeader {nonce = nextNonce}) $ genNextNonce nextNonce

validChain :: [Block] -> Bool
validChain chain = drop 1 (map hash chain ++ [0]) == map (parent . blockHdr) chain

verifyChain :: [Block] -> Maybe Hash
verifyChain [] = Just 0
verifyChain chain = if validChain chain then Just $ hash $ head chain else Nothing

verifyBlock :: Block -> Hash -> Maybe Hash
verifyBlock b@(Block hdr txs) parentHash = do
  guard (parent hdr == parentHash)
  guard (txroot hdr == treeHash (buildTree (coinbase hdr:txs)))
  guard (validNonce hdr)
  return (hash b)

data TransactionReceipt = TxReceipt {
  txrBlock :: Hash,
  txrProof :: MerkleProof Transaction
  } deriving Show


mineTransactions :: Miner -> Hash -> [Transaction] -> (Block, [TransactionReceipt])
mineTransactions miner parent txList = (resultBlock, receiptList) where
   resultBlock = mineBlock miner parent txList
   hashedResultBlock = hash resultBlock
   tree = buildTree $ coinbaseTx miner:txList
   receiptList = map ((\proof -> TxReceipt {txrBlock = hashedResultBlock, txrProof = proof}) . (\tx -> fromJust $ buildProof tx tree)) txList

validateReceipt :: TransactionReceipt -> BlockHeader -> Bool
validateReceipt txReceipt hdr = txrBlock txReceipt == hash hdr && verifyProof (txroot hdr) (txrProof txReceipt)

-- Printing functions provided from task description
pprHeader :: BlockHeader -> ShowS
pprHeader self@(BlockHeader parent cb txroot nonce)
  = pprV [ p ("hash", VH $ hash self)
         , p ("parent", VH $ parent)
         , p ("miner", VH $ txTo cb)
         , p ("root", VH txroot)
         , p ("nonce", nonce)
         ]
  where
    nl = showString "\n"
    p :: Show a => (String, a) -> ShowS
    p = showsPair

pprBlock :: Block -> ShowS
pprBlock (Block header txs)
 = pprHeader header
 . showChar '\n'
 . pprTxs (coinbase header:txs)

pprTx :: Transaction -> ShowS
pprTx tx@(Tx from to amount)
  = pprH [ showString "Tx#"
         , showsHash (hash tx)
         , p ("from", VH from)
         , p ("to", VH to)
         , p ("amount", amount)
         ]
  where
    p :: Show a => (String, a) -> ShowS
    p = showsPair

pprTxs :: [Transaction] -> ShowS
pprTxs = pprV . map pprTx


-- Example usage, required by task description
tx1 = Tx { txFrom = hash "Alice", txTo = hash "Bob", txAmount = 1*coin}
block0 = mineBlock (hash "Satoshi") 0 []
block1 = mineBlock (hash "Alice") (hash block0) []
block2 = mineBlock (hash "Charlie") (hash block1) [tx1]
chain = [block2, block1, block0]

-- | Chain verification
-- >>> verifyChain [block1, block2]
-- Nothing
--
-- >>> VH <$> verifyChain [block2,block1,block0]
-- Just 0x0dbea380

{- | Transaction Receipts
NB the following will not work in VS Code, see below
>>> let charlie = hash "Charlie"
>>> let (block, [receipt]) = mineTransactions charlie (hash block1) [tx1]
>>> block
BlockHeader {parent = 797158976, coinbase = Tx {txFrom = 0, txTo = 1392748814, txAmount = 50000}, txroot = 2327748117, nonce = 3}
Tx {txFrom = 2030195168, txTo = 2969638661, txAmount = 1000}
<BLANKLINE>

>>> receipt
TxReceipt {txrBlock = 230597504, txrProof = MerkleProof (Tx {txFrom = 2030195168, txTo = 2969638661, txAmount = 1000}) >0xbcc3e45a}
>>> validateReceipt receipt (blockHdr block)
True
-}

-- For VS Code, we need to use the "error trick"
{- Transaction Receipts
>>> let charlie = hash "Charlie"
>>> let (block, [receipt]) = mineTransactions charlie (hash block1) [tx1]
>>> error $ show block
BlockHeader {parent = 797158976, coinbase = Tx {txFrom = 0, txTo = 1392748814, txAmount = 50000}, txroot = 2327748117, nonce = 3}
Tx {txFrom = 2030195168, txTo = 2969638661, txAmount = 1000}

>>> receipt
TxReceipt {txrBlock = 230597504, txrProof = MerkleProof (Tx {txFrom = 2030195168, txTo = 2969638661, txAmount = 1000}) >0xbcc3e45a}

>>> validateReceipt receipt (blockHdr block)
True
-}

{- | Pretty printing
>>> runShows $ pprBlock block2
hash: 0x0dbea380
parent: 0x2f83ae40
miner: 0x5303a90e
root: 0x8abe9e15
nonce: 3
Tx# 0xbcc3e45a from: 0000000000 to: 0x5303a90e amount: 50000
Tx# 0x085e2467 from: 0x790251e0 to: 0xb1011705 amount: 1000

>>> runShows $ pprListWith pprBlock [block0, block1, block2]
hash: 0x70b432e0
parent: 0000000000
miner: 0x7203d9df
root: 0x5b10bd5d
nonce: 18
Tx# 0x5b10bd5d from: 0000000000 to: 0x7203d9df amount: 50000
hash: 0x2f83ae40
parent: 0x70b432e0
miner: 0x790251e0
root: 0x5ea7a6f0
nonce: 0
Tx# 0x5ea7a6f0 from: 0000000000 to: 0x790251e0 amount: 50000
hash: 0x0dbea380
parent: 0x2f83ae40
miner: 0x5303a90e
root: 0x8abe9e15
nonce: 3
Tx# 0xbcc3e45a from: 0000000000 to: 0x5303a90e amount: 50000
Tx# 0x085e2467 from: 0x790251e0 to: 0xb1011705 amount: 1000
-}

-- Now for VS Code:
{-
>>> error $ pprBlock block2 ""
hash: 0x0dbea380
parent: 0x2f83ae40
miner: 0x5303a90e
root: 0x8abe9e15
nonce: 3
Tx# 0xbcc3e45a from: 0000000000 to: 0x5303a90e amount: 50000
Tx# 0x085e2467 from: 0x790251e0 to: 0xb1011705 amount: 1000

>>> error $ pprListWith pprBlock [block0, block1, block2] ""
hash: 0x70b432e0
parent: 0000000000
miner: 0x7203d9df
root: 0x5b10bd5d
nonce: 18
Tx# 0x5b10bd5d from: 0000000000 to: 0x7203d9df amount: 50000
hash: 0x2f83ae40
parent: 0x70b432e0
miner: 0x790251e0
root: 0x5ea7a6f0
nonce: 0
Tx# 0x5ea7a6f0 from: 0000000000 to: 0x790251e0 amount: 50000
hash: 0x0dbea380
parent: 0x2f83ae40
miner: 0x5303a90e
root: 0x8abe9e15
nonce: 3
Tx# 0xbcc3e45a from: 0000000000 to: 0x5303a90e amount: 50000
Tx# 0x085e2467 from: 0x790251e0 to: 0xb1011705 amount: 1000
-}
