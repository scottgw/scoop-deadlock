module AST.Position 
    (
     Pos ()
    ,Line
    ,Column
    ,SourcePos

    ,sourceLine
    ,sourceColumn
    ,sourceName

    ,inheritPos

    ,attachPos
    ,attachPos'
    ,attachPosM
    ,attachPosBefore
    ,attachPosHere

    ,posFeatureName
    ,position
    ,contents
    ) where

import Control.Monad

import Text.Parsec
import Text.Parsec.ByteString

data Pos a = Pos String SourcePos a deriving (Eq)

instance Show a => Show (Pos a) where
    show p = show (position p) ++ "> " ++ show (contents p)

instance Functor Pos where
    fmap f (Pos fname s a) = Pos fname s (f a)

inheritPos :: (Pos a -> b) -> Pos a -> Pos b
inheritPos f a = attachPos (posFeatureName a) (position a) (f a)

attachPos :: String -> SourcePos -> a -> Pos a
attachPos = Pos

attachPos' = attachPos "noFeature"

attachPosM :: Monad m => m SourcePos -> m a -> m (Pos a)
attachPosM = liftM2 attachPos'

attachPosHere :: a -> Parser (Pos a)
attachPosHere a = flip attachPos' a `fmap` getPosition

attachPosBefore :: Parser a -> Parser (Pos a)
attachPosBefore = attachPosM getPosition

posFeatureName :: Pos a -> String
posFeatureName (Pos f _ _) = f

position :: Pos a -> SourcePos
position (Pos _ p _) = p

contents :: Pos a -> a
contents (Pos _ _ a) = a
