module Main exposing (..)

import Bootstrap.Controls.MultiSelect as MultiSelect exposing (..)
import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Events.Extra exposing (..)
import List

main : Program Never
main = App.program
  { view = view
  , init = init
  , update = update
  , subscriptions = \_ -> Sub.none
  }


type alias Model =
  { formValue : String
  , items: MultiSelect.Model Item Msg
  }


type alias Item =
  {name:String
  }


type Msg
  = NoOp
  | AddItem
  | UpdateForm String
  | MultiSelectMsg MultiSelect.Msg


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp -> (model, Cmd.none)
    AddItem ->
        { model
        | items = addItem model.items (Item model.formValue)
        , formValue = ""
        }
        ! []
    UpdateForm value ->
      { model | formValue = value } ! []
    MultiSelectMsg msg ->
      let
        (items', cmds) = MultiSelect.update msg model.items
      in
        {model | items = items'} ! [Cmd.map MultiSelectMsg cmds]


init : (Model, Cmd Msg)
init = (initialModel, Cmd.none)


initialModel : Model
initialModel =
  let
    items =
      [ Item "item1"
      , Item "item2"
      , Item "item3"
      , Item "item4"
      ]
    items' = List.map (\item -> (item, False)) items
    mapper = MultiSelectMsg
    name = \item -> item.name
  in
    { items = MultiSelect.init mapper name items'
    , formValue = ""
    }


view : Model -> Html Msg
view model =
  div
  []
  [ itemForm model.formValue
  , multiSelectButton model.items [] [ text "Items" ]
  , itemList <| MultiSelect.selectedItems model.items
  ]


item : Item -> Html Msg
item i =
  li
    [class "item"]
    [text i.name]


itemList : List Item -> Html Msg
itemList items =
  ul
    [class "item-list"]
    (List.map item items)


itemForm : String -> Html Msg
itemForm formValue = input
  [ value formValue
  , onInput UpdateForm
  , onEnter NoOp AddItem
  ]
  []
