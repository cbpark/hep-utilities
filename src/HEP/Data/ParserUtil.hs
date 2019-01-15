module HEP.Data.ParserUtil (skipTillEnd, parseEvent) where

import           Control.Monad.Trans.State.Strict (StateT (..))
import           Data.Attoparsec.ByteString       (skipWhile)
import           Data.Attoparsec.ByteString.Char8 hiding (skipWhile)
import           Data.ByteString.Char8            (ByteString)
import           Pipes
import qualified Pipes.Attoparsec                 as PA

skipTillEnd :: Parser ()
skipTillEnd = skipWhile (not . isEndOfLine) >> endOfLine

parseEvent :: Monad m => Parser a -> Producer ByteString m () -> Producer a m ()
parseEvent pf s = do (r, s') <- lift $ runStateT (PA.parse pf) s
                     case r of Just (Right ev) -> yield ev >> parseEvent pf s'
                               _               -> return ()
