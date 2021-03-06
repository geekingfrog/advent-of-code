{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Utils.Parser where

import qualified Control.Monad.Loops as Loops
import Data.Functor
import Text.Megaparsec
import Text.Megaparsec.Char

isEOF :: MonadParsec e s m => m Bool
isEOF = (try eof $> True) <|> pure False

isNewLine :: (MonadParsec e s m, Token s ~ Char) => m Bool
isNewLine = (try newline $> True) <|> pure False

parseLines :: (MonadParsec e s m, Token s ~ Char) => m a -> m [a]
parseLines p = (p <* char '\n') `Loops.untilM` isEOF

parseLine  :: (MonadParsec e s m, Token s ~ Char) => m a -> m [a]
parseLine = Loops.whileM (not <$> isNewLine)
