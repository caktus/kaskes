module Messages exposing (message)

import Json.Decode exposing (..)

type alias Message =
  { name : String
  , message : String
  , msgType : String
  }

message : Decoder Message
message = object3 Message
  ("name"    := string)
  ("message" := string)
  ("msgType" := string)
