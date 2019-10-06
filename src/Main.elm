module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Html exposing (Attribute, Html, div, input, text)
import Html.Attributes exposing (..)
import Html.Events as Events exposing (onInput)
import Json.Decode as Decode
import Process
import Task


attrId : String -> Html.Attribute msg
attrId =
    Html.Attributes.id


styles =
    List.map (\( n, v ) -> style n v)


condSingle : Bool -> v -> List v
condSingle b v =
    if b then
        [ v ]

    else
        []


cond : Bool -> m -> m -> m
cond b m1 m2 =
    if b then
        m1

    else
        m2


condMap : Bool -> (m -> m) -> m -> m
condMap b f m =
    cond b (f m) m


choice : Bool -> (m -> m) -> (m -> m) -> m -> m
choice b f g m =
    if b then
        f m

    else
        g m


main : Platform.Program () DropDown DropDownMsg
main =
    Browser.element
        { init = \() -> ( dropDownComponent.init, Cmd.none )
        , view = dropDownComponent.view
        , update = dropDownComponent.update
        , subscriptions = \_ -> Sub.none
        }


dropDownComponent : DropDownComponent
dropDownComponent =
    { init = init
    , update = update
    , view = drawDropDown
    }


type alias DropDownComponent =
    { init : DropDown
    , update : DropDownMsg -> DropDown -> ( DropDown, Cmd DropDownMsg )
    , view : DropDown -> Html DropDownMsg
    }


type ItemId
    = ItemId Int


type DropDownKeys
    = Tab
    | Tabback
    | Select
    | Down
    | Up
    | NoAction


type MenuDir
    = UpMenu
    | DownMenu


type UIPart
    = MenuArea
    | ButtonArea


type DropDownMsg
    = MouseEnter UIPart
    | Open MenuDir
    | MouseLeave UIPart
    | Close
    | HoverItem ItemId
    | SelectItem ItemId
    | ButtonKeyPress DropDownKeys
    | ItemFocused ItemId
    | NoOp


type alias Item =
    { value : String
    }


type alias DropDown =
    { open : Bool
    , selected : ItemId
    , hovered : Maybe ItemId
    , focused : Maybe ItemId
    , items : List Item
    , menuDir : MenuDir
    , hoveringBot : Bool
    , hoveringMenu : Bool
    }


type alias Activity =
    { focused : Bool
    , hovered : Bool
    }


init : DropDown
init =
    { open = False
    , selected = ItemId 0
    , hovered = Nothing
    , focused = Nothing
    , items = [ Item "Cat", Item "Dog", Item "Monkey" ]
    , menuDir = DownMenu
    , hoveringBot = False
    , hoveringMenu = False
    }


mapKey : Int -> Bool -> DropDownKeys
mapKey i b =
    case i of
        -- Enter
        13 ->
            Select

        -- Space
        32 ->
            Select

        -- Tab
        9 ->
            cond b Tabback Tab

        -- ArrowUp
        38 ->
            Up

        -- ArrowDown
        40 ->
            Down

        x ->
            Debug.log "key: " x
                |> always NoAction



-- UPDATE


openMenu : MenuDir -> DropDown -> DropDown
openMenu md dd =
    cond (not dd.open)
        { dd | open = True, menuDir = md, hovered = Nothing }
        dd


closeMenu : DropDown -> DropDown
closeMenu dd =
    cond dd.open
        { dd | open = False, hovered = Nothing, focused = Nothing }
        dd


hover : ItemId -> DropDown -> DropDown
hover i dd =
    { dd | hovered = Just i }


select : ItemId -> DropDown -> DropDown
select i dd =
    { dd | selected = i }


setFocus : ItemId -> DropDown -> DropDown
setFocus id dd =
    { dd | focused = Just id }


itemIdstr : ItemId -> String
itemIdstr (ItemId id) =
    "dd-mn-item-" ++ String.fromInt id


buttonIdStr : String
buttonIdStr =
    "dd-btn"


menuIdStr : String
menuIdStr =
    "dd-mn"


attemptFocus : String -> DropDownMsg -> Cmd DropDownMsg
attemptFocus id msg =
    Task.attempt
        (Result.map (\() -> msg)
            >> Result.withDefault NoOp
        )
        (Dom.focus id)


attemptFocusItem : ItemId -> Cmd DropDownMsg
attemptFocusItem id =
    attemptFocus (itemIdstr id) (ItemFocused id)


openingDirection : Dom.Element -> MenuDir
openingDirection e =
    let
        start =
            e.element.y - e.viewport.y

        end =
            start + e.element.height
    in
    if start >= 0 && end > e.viewport.height then
        UpMenu

    else
        DownMenu


attempOpenMenu : Cmd DropDownMsg
attempOpenMenu =
    Task.attempt
        (Result.map (openingDirection >> Open)
            >> Result.withDefault NoOp
        )
        (Dom.getElement menuIdStr)


attemptCloseMenu : Cmd DropDownMsg
attemptCloseMenu =
    Process.sleep 0
        |> Task.perform (always Close)


nextItem : DropDown -> ItemId -> ItemId
nextItem dd (ItemId id) =
    ItemId (Basics.min (id + 1) (List.length dd.items))


prevItem : DropDown -> ItemId -> ItemId
prevItem dd (ItemId id) =
    ItemId (Basics.max (id - 1) 0)


firstItem : ItemId
firstItem =
    ItemId 0


itemActivity : DropDown -> ItemId -> Activity
itemActivity dd id =
    { focused = dd.focused == Just id
    , hovered = dd.hovered == Just id
    }


isMouseOvered : DropDown -> Bool
isMouseOvered dd =
    dd.hoveringBot || dd.hoveringMenu


hoverPart : Bool -> UIPart -> DropDown -> DropDown
hoverPart b part dd =
    case part of
        MenuArea ->
            { dd | hoveringMenu = b }

        ButtonArea ->
            { dd | hoveringBot = b }


update : DropDownMsg -> DropDown -> ( DropDown, Cmd DropDownMsg )
update msg dd =
    case msg of
        MouseEnter part ->
            ( hoverPart True part dd
            , cond (isMouseOvered dd) Cmd.none attempOpenMenu
            )

        MouseLeave part ->
            let
                newDD =
                    hoverPart False part dd
                        |> condMap (part == MenuArea) (\d -> { d | hovered = Nothing })
            in
            ( newDD
            , cond (isMouseOvered newDD) Cmd.none attemptCloseMenu
            )

        Open menuDir ->
            ( openMenu menuDir dd, Cmd.none )

        Close ->
            ( cond (isMouseOvered dd || dd.focused /= Nothing) dd (closeMenu dd), Cmd.none )

        HoverItem i ->
            ( hover i dd, Cmd.none )

        SelectItem i ->
            ( dd
                |> select i
                |> closeMenu
            , dd.focused
                |> Maybe.map (always (attemptFocus buttonIdStr NoOp))
                |> Maybe.withDefault Cmd.none
            )

        ItemFocused id ->
            ( setFocus id dd, Cmd.none )

        ButtonKeyPress Select ->
            if dd.open then
                if dd.focused == Nothing then
                    ( dd, attemptFocusItem firstItem )

                else
                    ( closeMenu dd, attemptFocus buttonIdStr NoOp )

            else
                ( dd, Cmd.batch [ attempOpenMenu, attemptFocusItem firstItem ] )

        ButtonKeyPress Tab ->
            ( closeMenu dd, Cmd.none )

        ButtonKeyPress Down ->
            let
                focusedItem =
                    dd.focused
                        |> Maybe.withDefault firstItem
                        |> nextItem dd
            in
            ( dd, attemptFocusItem focusedItem )

        ButtonKeyPress Up ->
            let
                focusedItem =
                    dd.focused
                        |> Maybe.withDefault firstItem
                        |> prevItem dd
            in
            ( dd, attemptFocusItem focusedItem )

        ButtonKeyPress NoAction ->
            ( dd, Cmd.none )

        NoOp ->
            Debug.log "sad" ( dd, Cmd.none )



-- VIEW


selectItem : ItemId -> DropDown -> Maybe Item
selectItem (ItemId id) dd =
    List.drop id dd.items
        |> List.head


buttonStyle : List (Html.Attribute DropDownMsg)
buttonStyle =
    styles
        [ ( "background-color", "#3498DB" )
        , ( "color", "white" )
        , ( "padding", "16px" )
        , ( "font-size", "16px" )
        , ( "border", "none" )
        , ( "cursor", "default" )
        ]


itemStyle : Activity -> List (Html.Attribute DropDownMsg)
itemStyle act =
    styles
        ([ ( "color", "black" )
         , ( "padding", "12px 16px" )
         , ( "text-decoration", "none" )
         , ( "display", "block" )
         ]
            |> condMap act.hovered ((++) [ ( "background-color", "#ddd" ) ])
            |> condMap act.focused ((++) [ ( "outline", "1px solid red" ) ])
        )


menuStyle : Bool -> MenuDir -> List (Html.Attribute DropDownMsg)
menuStyle visible dir =
    styles
        ([ ( "bottom", cond (dir == UpMenu) "50px" "50" )
         , ( "background-color", "#f1f1f1" )
         , ( "min-width", "160px" )
         , ( "box-shadow", "0px 8px 16px 0px rgba(0,0,0,0.2)" )
         , ( "z-index", "1" )
         , ( "position", "absolute" )
         ]
            ++ cond visible [] [ ( "visibility", "hidden" ) ]
        )


componentStyle : List (Html.Attribute DropDownMsg)
componentStyle =
    styles
        [ ( "position", "relative" )
        , ( "display", "inline-block" )
        ]


dropDownButton : DropDown -> Html DropDownMsg
dropDownButton dd =
    div
        (buttonStyle
            ++ [ tabindex 0 ]
            ++ [ Events.onMouseEnter (MouseEnter ButtonArea)
               , Events.onMouseLeave (MouseLeave ButtonArea)
               , attrId buttonIdStr
               ]
        )
        [ text
            (selectItem dd.selected dd
                |> Maybe.map .value
                |> Maybe.withDefault "<Unknown>"
            )
        ]


dropDownItem : Activity -> ItemId -> Item -> Html DropDownMsg
dropDownItem act id itm =
    Html.a
        (itemStyle act
            ++ [ href "#"
               , Html.Attributes.id (itemIdstr id)
               , Events.onMouseEnter (HoverItem id)
               , Events.onClick (SelectItem id)
               ]
        )
        [ text itm.value ]


dropDownMenu : DropDown -> Html DropDownMsg
dropDownMenu dd =
    div
        (menuStyle dd.open dd.menuDir
            ++ [ Html.Attributes.tabindex -1
               , Events.onMouseLeave (MouseLeave MenuArea)
               , Events.onMouseEnter (MouseEnter MenuArea)
               , attrId menuIdStr
               ]
        )
        (List.indexedMap (\i -> dropDownItem (itemActivity dd (ItemId i)) (ItemId i)) dd.items)


drawDropDown : DropDown -> Html DropDownMsg
drawDropDown dd =
    div
        (styles
            [ ( "position", "absolute" )
            , ( "display", "block" )
            , ( "position", "fixed" )
            , ( "top", "50%" )
            ]
        )
        [ div
            (componentStyle
                ++ [ Events.on "keydown"
                        (Decode.map2
                            (\i b -> ButtonKeyPress (mapKey i b))
                            (Decode.field "keyCode" Decode.int)
                            (Decode.field "shiftKey" Decode.bool)
                        )
                   ]
            )
            [ dropDownButton dd
            , dropDownMenu dd
            ]
        ]
