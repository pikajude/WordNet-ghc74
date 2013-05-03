module NLP.WordNet.Consts where

import Data.String
import System.FilePath (joinPath)

makePath :: [FilePath] -> FilePath
makePath = joinPath

dictDir :: IsString a => a
defaultPath :: IsString a => a
defaultBin :: IsString a => a

#if defined (UNIX)
dictDir         = "/dict"
defaultPath     = "/usr/local/WordNet-2.0/dict"
defaultBin      = "/usr/local/WordNet-2.0/bin"

#elif defined (PC)
dictDir         = "\\dict"
defaultPath	= "c:\\WordNet 2.0\\dict"
defaultBin      = "c:\\WordNet 2.0\\bin"

#elif defined (MAC)
dictDir         = ":Database"
defaultPath     = ":Database"
defaultBin      = ":"

#else
-- guess unix style
dictDir         = "/dict"
defaultPath     = "/usr/local/WordNet-2.0/dict"
defaultBin      = "/usr/local/WordNet-2.0/bin"

#endif
