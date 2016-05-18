module Html.Events.Extra exposing
  ( onEnter
  )

import Html exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json

onEnter : msg -> msg -> Attribute msg
onEnter fail success =
  let
    onKeyup code =
      if code == 13 then
        success
      else
        fail
  in
    on "keyup" (Json.map onKeyup keyCode)
