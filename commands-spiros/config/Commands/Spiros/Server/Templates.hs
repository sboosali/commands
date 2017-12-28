{-# LANGUAGE QuasiQuotes, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures  #-}
module Commands.Spiros.Server.Templates where

import           Commands.Extra (displayAddress)
import           Text.InterpolatedString.Perl6

curl_ExampleRequest address = [qq|curl  -X POST  -H "Content-Type: application/json"  -d '["par","round","grave","camel","lit","async","break","break","action"]'  "http://{displayAddress address}/recognition/"|]
