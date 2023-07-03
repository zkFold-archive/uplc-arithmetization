{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module IO where

import           Data.Aeson                  (decode, encode)
import           Data.ByteString.Lazy        (readFile, writeFile)
import           Data.Maybe                  (fromMaybe)
import           Prelude                     hiding ((!!), drop, take, readFile, writeFile)

import           CEK                         (cekInit, cekTracePrint)
import           CEKPolyData                 (cekPolyData)
import           Types.Constant              (loadConstantArray)
import           Types.PolyData              (PolyDataItem)
import           Types.Term                  (loadTermArray)

loadPolyDataUnsafe :: FilePath -> IO [(PolyDataItem, PolyDataItem, PolyDataItem)]
loadPolyDataUnsafe path = fromMaybe [] . decode <$> readFile path

processProgram :: IO ()
processProgram = do
    constants <- loadConstantArray <$> loadPolyDataUnsafe "constants.json"
    terms     <- loadTermArray constants <$> loadPolyDataUnsafe "terms.json"

    let program = fst $ head terms
        witness = cekPolyData constants program
    cekTracePrint $ cekInit program

    writeFile "witness.json" $ encode witness