{-# Language DeriveGeneric, DeriveAnyClass, FlexibleInstances, StandaloneDeriving #-}

import           Criterion.Main
import           Control.DeepSeq
import           Data.Functor.Compose (getCompose)
import Data.List (intercalate)
import qualified Language.WebIDL.Grammar as G
import Language.WebIDL.AST
import Language.WebIDL.Parser
import Language.WebIDL.PPrint
import Control.Monad.IO.Class (liftIO)

import Prelude hiding (Enum)

testFiles :: [FilePath]
testFiles = [ "examples/webgl.idl"
            , "examples/fileapi.idl"
            , "examples/callback.idl" ]

main :: IO ()
main = do tests <- concat <$> traverse readFile testFiles
          defaultMain
            [ bench "Parsing WebIDL files with Parsec" (nf (map printDef . either (error "failed parse") id . parseIDL) tests),
              bench "Parsing WebIDL files with Grampa" (nf (map printDef . either (error "failed parse") concat . G.parseIDL) tests)
            ]
