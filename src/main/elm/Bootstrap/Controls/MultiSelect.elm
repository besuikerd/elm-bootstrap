module Bootstrap.Controls.MultiSelect exposing
  ( Model
  , Msg
  , update
  , init
  , multiSelect
  , multiSelectButton
  , filterItems
  , items
  , selectedItems
  , notSelectedItems
  , addItem
  , updateItems
  , selected
  , item
  )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra as List'

type alias Attributes a = List (Attribute a)
type alias Children a = List (Html a)
type alias HtmlElement a =
  Attributes a -> Children a -> Html a
type alias Entry item = (item, Bool, Int)


type alias Model item msg =
  { open: Bool
  , uid : Int
  , items: List (Entry item)
  , mapper: (Msg -> msg)
  , name : (item -> String)
  }


item : Entry item -> item
item (item, _, _) = item


selected : Entry item -> Bool
selected (_, selected, _) = selected


itemId : Entry item -> Int
itemId (_, _, id) = id


type Msg
  = NoOp
  | ToggleOpen
  | CheckItem Int


filterItems : (Entry item -> Bool) -> Model item msg -> List item
filterItems f model = List.map item <| List.filter f <| model.items


items : Model item msg -> List item
items = filterItems <| always True


selectedItems : Model item msg -> List item
selectedItems = filterItems selected


notSelectedItems : Model item msg -> List item
notSelectedItems = filterItems (not << selected)


updateItems : Model item msg -> List (Entry item) -> Model item msg
updateItems model items =
  { model | items = items }


addItem: Model item msg -> item -> Model item msg
addItem model item =
  { model
  | items = model.items ++ [(item, False, model.uid)]
  , uid = model.uid + 1
  }


init : (Msg -> msg) -> (item -> String) -> List (item, Bool) -> Model item msg
init mapper name items =
  let
    uid = List.length items
    items' = List.map (\((item, selected), i) -> (item, selected, i)) <| List'.zip items [0..uid]
  in
    { open = False
    , uid = uid
    , items = items'
    , mapper = mapper
    , name = name
    }


update : Msg -> Model item msg -> (Model item msg, Cmd Msg)
update msg model =
  case msg of
    NoOp -> (model, Cmd.none)
    ToggleOpen ->
      { model | open = not model.open } ! []
    CheckItem id ->
      let
        updateItem ((item, checked, id') as entry) = if id == id' then (item, not checked, id') else entry
      in
        { model
        | items = List.map updateItem model.items
        } ! []


multiSelect : Model item msg -> HtmlElement msg -> Attributes msg -> Children msg -> Html msg
multiSelect model elem attributes children =
  div
    []
    ( [ elem
        ( attributes ++ [onClick (model.mapper ToggleOpen)] )
        children
      ]
    ++ if model.open then [multiSelectList model.mapper model.name model.items] else []
    )


multiSelectList : (Msg -> msg) -> (item -> String) -> List (Entry item) -> Html msg
multiSelectList mapper name items =
  ul
    []
    (List.map (multiSelectItem mapper name) items)


multiSelectItem : (Msg -> msg) -> (item -> String) -> Entry item -> Html msg
multiSelectItem mapper name entry =
  li
    [onClick <| mapper (CheckItem (itemId entry))]
    [ input
        [ type' "checkbox"
        , checked <| (selected entry)
        ]
        []
    , text <| name <| item entry
    , text <| toString <| itemId entry
    ]


multiSelectButton : Model item msg -> HtmlElement msg
multiSelectButton model = multiSelect model button
