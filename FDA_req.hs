{-#LANGUAGE OverloadedStrings #-}
module Main where 

import Network.HTTP.Conduit
import Data.Aeson.Lens
import Control.Lens
-- import Data.Text
import qualified Data.ByteString.Char8 as BC -- strict Char8 ByteString
import qualified Data.ByteString.Lazy.Char8 as BLC -- lazy Char8 ByteString

-- This program retrieves JSON from the Open FDA site, and parses it into adverse reactions and their associated count

-- url endpoint
url :: String
url = "https://api.fda.gov/drug/event.json?search=patient.drug.openfda.pharm_class_epc:\"nonsteroidal+anti-inflammatory+drug\"&count=patient.reaction.reactionmeddrapt.exact"
	
-- make the HTTP request and return a lazy Char8 ByteString
requestData :: IO BLC.ByteString
requestData = simpleHttp url

-- pierces a Monad m, extracts the lazy ByteString, converts it to a String and wraps it in the Monad
fromLazyToString :: Monad m => BLC.ByteString -> m [Char]
fromLazyToString a = return (BC.unpack (BLC.toStrict a))

{- Alright, if we've made it this far we have a JSON response from the server in String type. A prism
will take our string and drill down to the "results" array and extract the values as members -}
prismMembers json = return (json ^@.. key "results" . values. members)

-- request the JSON, convert it to a String, prism out the members
main = requestData >>= fromLazyToString >>= prismMembers