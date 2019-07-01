module Servant.API.MimeRender
  ( class MimeUnrender
  , mimeUnrender
  , class MimeRender
  , mimeRender
  ) where

import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseBody
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson)
import Data.Either (Either)
import Type.Proxy (Proxy)

class MimeUnrender ctype a where
  mimeUnrender :: Proxy a -> Proxy ctype -> { responseFormat :: ResponseBody.ResponseFormat ctype
                                            , decode :: ctype -> Either String a
                                            }

instance mimeUnrenderJson :: DecodeJson a => MimeUnrender Json a where
  mimeUnrender _ _ = { responseFormat: ResponseBody.json
                     , decode: decodeJson
                     }

class MimeRender ctype a where
  mimeRender :: Proxy a -> Proxy ctype -> { print :: ctype -> RequestBody.RequestBody
                                          , encode :: a -> ctype
                                          }

instance mimeRenderJson :: EncodeJson a => MimeRender Json a where
  mimeRender _ _ = { print: RequestBody.json
                   , encode: encodeJson
                   }
