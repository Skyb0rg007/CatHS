{-# LANGUAGE KindSignatures #-}

module MultiRec.Constructor
    ( Constructor (..)
    , Fixity (..)
    , Associativity (..)
    ) where

class Constructor c where
    conName :: t c (f :: (* -> *) -> * -> *) (r :: * -> *) ix -> String
    conFixity :: t c (f :: (* -> *) -> * -> *) (r :: * -> *) ix -> Fixity
    conFixity = const Prefix

data Fixity = Prefix | Infix Associativity Int
    deriving (Show, Eq, Ord, Read)

data Associativity = LeftAssociative | RightAssociative | NonAssociative
    deriving (Show, Eq, Ord, Read)
