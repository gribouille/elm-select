module Select exposing
    ( Config, Item, ItemId(..), Show(..), State, default, fromId, init, values
    , view
    )

{-| Select component (in progress).


# Data

@docs Config, Item, ItemId, Show, State, default, fromId, init, values


# View

@docs view

-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import List.Extra exposing (find)
import Maybe exposing (withDefault)


{-| TODO
-}
type alias State =
    { show : Bool -- show the values
    , selected : List ItemId
    , search : String
    }


{-| TODO
-}
values : List Item -> State -> List String
values items state =
    state.selected
        |> List.filterMap (\id -> find (\item -> item.id == id) items)
        |> List.map .value


{-| TODO
-}
init : List ItemId -> State
init l =
    State False l ""


{-| TODO
-}
type Show
    = Id
    | Value
    | Title


{-| TODO
-}
type alias Config msg =
    { pipe : State -> msg
    , showSelected : List Show
    , showItem : List Show
    , items : List Item
    }


{-| TODO
-}
type alias Item =
    { id : ItemId
    , value : String
    , title : String
    }


{-| TODO
-}
fromId : ItemId -> Item
fromId id =
    Item id (toStr id) (toStr id)


{-| TODO
-}
type ItemId
    = ItemIdI Int
    | ItemIdS String


{-| TODO
-}
default : (State -> msg) -> List Item -> Config msg
default pipe items =
    Config pipe [ Value ] [ Value, Title ] items


{-| TODO
-}
view : Config msg -> State -> Html msg
view config state =
    let
        onShow =
            config.pipe { state | show = not state.show }

        onUnselect =
            \id -> config.pipe { state | selected = List.filter ((/=) id) state.selected }

        onSelect =
            \id ->
                config.pipe
                    { state
                        | selected =
                            if List.any ((==) id) state.selected then
                                List.filter ((/=) id) state.selected

                            else
                                id :: state.selected
                    }

        onSearch =
            \s -> config.pipe { state | search = s }

        selected =
            List.map (viewSelected config.showSelected onUnselect) <|
                List.map
                    (\id ->
                        withDefault (fromId id) (find (\m -> m.id == id) config.items)
                    )
                    state.selected

        searchFilterFn =
            List.filter (\item -> String.contains state.search (showItem config.showItem item))

        setupViewItemFn =
            \item -> viewItem config.showItem onSelect (List.member item.id state.selected) item

        missingSelectedValues =
            List.map fromId <|
                List.filter (\id -> not <| List.any (\m -> m.id == id) config.items) state.selected

        items =
            List.map setupViewItemFn <| searchFilterFn <| config.items ++ missingSelectedValues
    in
    div [ class "gribouille-select" ]
        [ div [ class "values" ] <|
            selected
                ++ [ span
                        [ class "search" ]
                        [ input
                            [ type_ "text"
                            , value state.search
                            , onFocus onShow
                            , onInput onSearch
                            ]
                            []
                        ]
                   , a
                        [ class "expand", href "", onClickP onShow ]
                        [ icon <| iff state.show "arrow_drop_up" "arrow_drop_down" ]
                   ]
        , div [ class <| "items " ++ iff state.show "overlay" "is-hidden" ]
            [ ul [] items ]
        ]


viewSelected : List Show -> (ItemId -> msg) -> Item -> Html msg
viewSelected show onUnselect item =
    span [ class "tag is-link" ]
        [ text (showItem show item)
        , button [ class "delete is-small", onClick (onUnselect item.id) ] []
        ]


showItem : List Show -> Item -> String
showItem opts item =
    List.foldl (\o acc -> acc ++ iff (String.isEmpty acc) "" " - " ++ showItem_ item o) "" opts


toStr : ItemId -> String
toStr id =
    case id of
        ItemIdI i ->
            String.fromInt i

        ItemIdS s ->
            s


showItem_ : Item -> Show -> String
showItem_ item show =
    case show of
        Id ->
            toStr item.id

        Value ->
            item.value

        Title ->
            item.title


viewItem : List Show -> (ItemId -> msg) -> Bool -> Item -> Html msg
viewItem opts onSelect isSelected item =
    li [ class (iff isSelected "selected" "") ]
        [ a [ href "", onClickP (onSelect item.id) ] [ text (showItem opts item) ] ]


iff : Bool -> String -> String -> String
iff c t f =
    if c then
        t

    else
        f


onClickP : msg -> Attribute msg
onClickP m =
    custom "click" <|
        Json.succeed
            { message = m
            , stopPropagation = True
            , preventDefault = True
            }


icon : String -> Html msg
icon name =
    i [ class "material-icons ui" ] [ text name ]
