module InfiniteGallery exposing
    ( Config, defaultConfig
    , init, update, view
    , previous, next, goTo, setIndex
    , getCurrentIndex
    , Gallery, Msg
    )

{-|


# Configuration

@docs Config, defaultConfig


# Framework

@docs init, update, view


# Control

@docs previous, next, goTo, setIndex


# Retrieve

@docs getCurrentIndex


# Definitions

@docs Gallery, Msg

-}

import Html exposing (Attribute, Html, div, node, text)
import Html.Attributes exposing (class, classList, id, style, type_)
import Html.Events exposing (on, preventDefaultOn)
import Html.Lazy exposing (lazy2)
import Json.Decode as Decode exposing (at, field, float, map, oneOf)
import Process exposing (sleep)
import Task exposing (perform)


{-| The model of a Gallery
You can create a new Gallery using the init function.
-}
type Gallery
    = Gallery Size Config CurrentSlide DragState Slides


{-| The configuration options of a Gallery.
These can be passed in while initializing a new Gallery.
-}
type alias Config =
    { rootClassName : String
    , id : String
    , transitionSpeed : Int
    , swipeOffset : Int
    }


{-| A convenience function that returns some default configuration options.
-}
defaultConfig : Config
defaultConfig =
    { rootClassName = "BellroyGallery"
    , id = "Gallery_0"
    , transitionSpeed = 300
    , swipeOffset = 150
    }


{-| Represents the size of the Gallery
-}
type alias Size =
    { width : String
    , height : String
    }


{-| Slides are a List of their Index and Html elements
-}
type alias Slides =
    List ( Int, Html Msg )


{-| The current active slide
-}
type alias CurrentSlide =
    Int


{-| Is the user dragging? What was the intiial position and what is the current position of the mouse, touch object ?
-}
type DragState
    = NotDragging
    | Dragging PosX PosX


{-| Represents a position on an X axis
-}
type PosX
    = PosX Int


{-| The Gallery's internal Msg's
-}
type Msg
    = DragStart PosX
    | DragAt PosX
    | DragEnd
    | Next
    | Previous
    | GoTo Int
    | SetIndex Int
    | SetTransitionSpeed Int
    | Batch (List ( Float, Msg ))


{-| Create a new Gallery, with given size, config and slides
-}
init :
    { width : String, height : String }
    -> Config
    -> List (Html Msg)
    -> Gallery
init size config =
    Gallery size config 0 NotDragging
        << List.indexedMap Tuple.pair


{-| Update the gallery with given Msg.
You would need to wrap and unwrap these Msg's inside your own update function.

Example:

    type Msg
        = MsgForGallery Gallery.Msg

    update : Msg -> Model -> Model
    update msg model =
        case msg of
            MsgForGallery msg ->
                { model | gallery = Gallery.update msg model.gallery }

-}
update : Msg -> Gallery -> ( Gallery, Cmd Msg )
update msg ((Gallery size config currentSlide dragState slides) as gallery) =
    let
        wait ms msg_ =
            perform (always msg_) (sleep ms)

        frame =
            1000 / 30
    in
    case msg of
        DragStart posX ->
            ( Gallery size config currentSlide (Dragging posX posX) slides
            , Cmd.none
            )

        DragAt posX ->
            case dragState of
                NotDragging ->
                    ( gallery, Cmd.none )

                Dragging startX _ ->
                    ( Gallery size config currentSlide (Dragging startX posX) slides
                    , Cmd.none
                    )

        DragEnd ->
            case dragState of
                NotDragging ->
                    ( Gallery size config currentSlide NotDragging slides
                    , Cmd.none
                    )

                Dragging (PosX startX) (PosX currentX) ->
                    if (startX - currentX) > config.swipeOffset then
                        update Next gallery

                    else if abs (startX - currentX) > config.swipeOffset then
                        update Previous gallery

                    else
                        ( Gallery size config currentSlide NotDragging slides
                        , Cmd.none
                        )

        Next ->
            if currentSlide >= (List.length slides - 1) then
                update
                    (Batch
                        [ ( toFloat config.transitionSpeed, SetIndex (currentSlide + 1) )
                        , ( frame, SetIndex 0 )
                        ]
                    )
                    gallery

            else
                ( Gallery size config (currentSlide + 1) NotDragging slides
                , Cmd.none
                )

        Previous ->
            if currentSlide <= 0 then
                update
                    (Batch
                        [ ( 300, SetIndex (currentSlide - 1) )
                        , ( frame, SetIndex (List.length slides - 1) )
                        ]
                    )
                    gallery

            else
                ( Gallery size config (currentSlide - 1) NotDragging slides
                , Cmd.none
                )

        SetIndex index ->
            update
                (Batch
                    [ ( frame, SetTransitionSpeed 0 )
                    , ( frame, GoTo index )
                    , ( frame, SetTransitionSpeed config.transitionSpeed )
                    ]
                )
                gallery

        GoTo index ->
            ( Gallery size config index NotDragging slides
            , Cmd.none
            )

        SetTransitionSpeed timeInMs ->
            ( Gallery size { config | transitionSpeed = timeInMs } currentSlide dragState slides
            , Cmd.none
            )

        Batch (( ms, firstMsg ) :: listOfCmds) ->
            update firstMsg gallery
                |> Tuple.mapSecond (\cmd -> Cmd.batch [ wait ms (Batch listOfCmds), cmd ])

        Batch [] ->
            ( gallery, Cmd.none )


{-| Go to the next slide
-}
next : Gallery -> ( Gallery, Cmd Msg )
next gallery =
    update Next gallery


{-| Go to the previous slide
-}
previous : Gallery -> ( Gallery, Cmd Msg )
previous gallery =
    update Previous gallery


{-| Go to a specific slide, animated
-}
goTo : Int -> Gallery -> ( Gallery, Cmd Msg )
goTo index gallery =
    update (GoTo index) gallery


{-| Go to a specific slide, direclty
-}
setIndex : Int -> Gallery -> ( Gallery, Cmd Msg )
setIndex index gallery =
    update (SetIndex index) gallery


{-| Retrieve the current displayed Slide index
-}
getCurrentIndex : Gallery -> Int
getCurrentIndex (Gallery _ _ currentSlide _ _) =
    currentSlide


{-| Render your gallery
e.g.

Example:

    type Msg
        = MsgForGallery Gallery.Msg

    view : Model -> Html Msg
    view model =
        Html.div []
            [ Gallery.view model.gallery
                |> Html.map MsgForGallery
            ]

-}
view : Gallery -> Html Msg
view ((Gallery size config currentSlide dragState slides) as gallery) =
    let
        viewSlide ( index, slideHtml ) =
            div
                [ classList
                    [ ( prefixClassName config "Slides_Slide", True )
                    , ( "active", currentSlide == index )
                    ]
                ]
                [ slideHtml
                ]
    in
    div
        [ class config.rootClassName
        , id config.id
        , style "width" size.width
        , style "height" size.height
        ]
        [ viewStylesheet gallery
        , div [ class <| prefixClassName config "Wrapper" ]
            [ div
                ([ class <| prefixClassName config "Slides"
                 , classList
                    [ ( prefixClassName config "Slides--dragging"
                      , isDragging dragState
                      )
                    ]
                 ]
                    ++ slidesEvents dragState
                    ++ dragOffset dragState
                )
              <|
                List.map viewSlide <|
                    List.filterMap identity <|
                        ([ List.head <| List.reverse slides ]
                            ++ List.map Just slides
                            ++ [ List.head slides ]
                        )
            ]
        ]


{-| Determine if the user is currently dragging a slide
-}
isDragging : DragState -> Bool
isDragging dragState =
    case dragState of
        Dragging _ _ ->
            True

        NotDragging ->
            False


{-| Apply the users drag offset based on the dragState
-}
dragOffset : DragState -> List (Attribute Msg)
dragOffset dragState =
    case dragState of
        Dragging (PosX startX) (PosX currentX) ->
            if (currentX - startX) == 0 then
                [ style "transform" "translateX(0)" ]

            else
                [ style "transform" ("translateX(" ++ String.fromInt (currentX - startX) ++ "px)") ]

        NotDragging ->
            [ style "transform" "translateX(0)" ]


{-| Create all the slider listeners required to handle the DragState |
-}
slidesEvents : DragState -> List (Attribute Msg)
slidesEvents dragState =
    [ on "mousedown" (Decode.map DragStart decodePosX)
    , on "touchstart" (Decode.map DragStart decodePosX)
    ]
        ++ (if isDragging dragState then
                [ preventDefaultOn "mousemove"
                    (Decode.map (\posX -> ( DragAt posX, True )) decodePosX)
                , preventDefaultOn "touchmove"
                    (Decode.map (\posX -> ( DragAt posX, True )) decodePosX)
                , on "mouseup" (Decode.succeed DragEnd)
                , on "mouseleave" (Decode.succeed DragEnd)
                , on "touchend" (Decode.succeed DragEnd)
                , on "touchcancel" (Decode.succeed DragEnd)
                ]

            else
                []
           )


{-| Extract the PosX from a mouse or touch event
-}
decodePosX : Decode.Decoder PosX
decodePosX =
    let
        decoder =
            Decode.map PosX <|
                Decode.field "pageX" (Decode.map floor Decode.float)
    in
    Decode.oneOf
        [ decoder
        , Decode.at [ "touches", "0" ] decoder
        ]


{-| Prefix a className with the configured rootClassName
-}
prefixClassName : Config -> String -> String
prefixClassName { rootClassName } className =
    if className == "" then
        rootClassName

    else
        String.join "__" [ rootClassName, className ]


{-| Render the Gallery's stylesheet
-}
viewStylesheet : Gallery -> Html Msg
viewStylesheet ((Gallery size config currentSlide dragState slides) as gallery) =
    let
        amountOfSlides =
            List.length slides + 2

        styles =
            [ ( ""
              , []
              )
            , ( "Wrapper"
              , [ ( "position", "relative" )
                , ( "overflow", "hidden" )
                , ( "width", "100%" )
                , ( "height", "100%" )
                ]
              )
            , ( "Slides"
              , [ ( "position", "absolute" )
                , ( "left", String.fromInt ((currentSlide + 1) * -100) ++ "%" )
                , ( "top", "0" )
                , ( "width", "" ++ String.fromInt (amountOfSlides * 100) ++ "%" )
                , ( "height", "100%" )
                , ( "display", "grid" )
                , ( "grid-template-columns", "repeat(" ++ String.fromInt amountOfSlides ++ ", 1fr)" )
                , ( "transition", "left " ++ String.fromInt config.transitionSpeed ++ "ms ease, transform " ++ String.fromInt (config.transitionSpeed // 2) ++ "ms linear" )
                , ( "cursor", "grab" )
                ]
              )
            , ( "Slides--dragging"
              , [ ( "transition", "none" )
                ]
              )
            , ( "Slides_Slide"
              , [ ( "max-width", "100%" )
                , ( "max-height", "100%" )
                , ( "overflow", "hidden" )
                , ( "position", "relative" )
                , ( "user-drag", "none" )
                , ( "user-select", "none" )
                , ( "-webkit-user-select", "none" )
                , ( "-moz-user-select", "none" )
                , ( "-ms-user-select", "none" )
                ]
              )
            ]

        renderStyleBlock ( className, rules ) =
            List.map (\( a, b ) -> String.join ":" [ a, b ]) rules
                |> String.join ";"
                |> (++) ("#" ++ config.id ++ " ." ++ prefixClassName config className ++ " { ")
                |> (\a -> a ++ "}")
                |> text
    in
    lazy2
        (\_ _ ->
            node
                "style"
                [ type_ "text/css" ]
            <|
                List.map renderStyleBlock styles
        )
        currentSlide
        amountOfSlides
