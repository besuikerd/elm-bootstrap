module Html.Bootstrap exposing
  ( glyphicon
  )

import Html exposing (..)
import Html.Attributes exposing (..)

glyphicon : String -> Html a
glyphicon glyph = span [class ("glyphicon glyphicon-" ++ glyph)][]
