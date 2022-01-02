module InfiniteGallery exposing
    ( Config, TransitionSpeed(..)
    , init, update, view
    , previous, next, goTo, setIndex
    , getCurrentIndex
    , Gallery, Msg(..)
    )

{-|


# Configuration

@docs Config, TransitionSpeed


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
import Json.Decode as Decode exposing (float, map, oneOf)
import Process exposing (sleep)
import Task exposing (perform)


{-| The model of a Gallery
You can create a new Gallery using the init function.
-}
type Gallery
    = Gallery Size Config CurrentSlide DragState Slides TransitionSpeed


{-| The configuration options of a Gallery.
These can be passed in while initializing a new Gallery.
-}
type alias Config =
    { rootClassName : String
    , id : String
    , transitionSpeedWhenAdvancing : TransitionSpeed
    , enableDrag : Bool
    , swipeOffset : Int
    , initialSlide : Int
    }


{-| A convenience function that returns some default configuration options.
-}
defaultConfig : Config
defaultConfig =
    { rootClassName = "InfiniteGallery"
    , id = "InfiniteGallery0"
    , transitionSpeedWhenAdvancing = TransitionSpeed 300
    , enableDrag = True
    , swipeOffset = 150
    , initialSlide = 0
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


{-| A number of milliseconds to spend transitioning between indices.
-}
type TransitionSpeed
    = TransitionSpeed Int


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
    | SetTransitionSpeed TransitionSpeed
    | Batch (List ( TransitionSpeed, Msg ))
    | TransitionEnd


{-| Create a new Gallery, with given size, config and slides
-}
init :
    { width : String, height : String }
    -> Config
    -> List (Html Msg)
    -> Gallery
init size config slides =
    Gallery
        size
        config
        config.initialSlide
        NotDragging
        (List.indexedMap Tuple.pair slides)
        config.transitionSpeedWhenAdvancing


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
update msg ((Gallery size config currentSlide dragState slides transitionSpeed) as gallery) =
    let
        wait ms msg_ =
            perform (always msg_) (sleep ms)

        frame =
            TransitionSpeed (1000 // 30)
    in
    case msg of
        DragStart posX ->
            ( Gallery size config currentSlide (Dragging posX posX) slides transitionSpeed
            , Cmd.none
            )

        DragAt posX ->
            case dragState of
                NotDragging ->
                    ( gallery, Cmd.none )

                Dragging startX _ ->
                    ( Gallery size config currentSlide (Dragging startX posX) slides transitionSpeed
                    , Cmd.none
                    )

        DragEnd ->
            case dragState of
                NotDragging ->
                    ( Gallery size config currentSlide NotDragging slides transitionSpeed
                    , Cmd.none
                    )

                Dragging (PosX startX) (PosX currentX) ->
                    if (startX - currentX) > config.swipeOffset then
                        ( gallery, Task.perform (always Next) (Task.succeed ()) )

                    else if abs (startX - currentX) > config.swipeOffset then
                        ( gallery, Task.perform (always Previous) (Task.succeed ()) )

                    else
                        ( Gallery size config currentSlide NotDragging slides transitionSpeed
                        , Cmd.none
                        )

        Next ->
            ( Gallery size config (currentSlide + 1) NotDragging slides transitionSpeed
            , Cmd.none
            )

        Previous ->
            ( Gallery size config (currentSlide - 1) NotDragging slides transitionSpeed
            , Cmd.none
            )

        SetIndex index ->
            update
                (Batch
                    [ ( frame, SetTransitionSpeed (TransitionSpeed 0) )
                    , ( frame, GoTo index )
                    , ( frame, SetTransitionSpeed config.transitionSpeedWhenAdvancing )
                    ]
                )
                gallery

        GoTo index ->
            ( Gallery size config index NotDragging slides transitionSpeed
            , Cmd.none
            )

        SetTransitionSpeed timeInMs ->
            ( Gallery size config currentSlide dragState slides timeInMs
            , Cmd.none
            )

        Batch (( TransitionSpeed ms, firstMsg ) :: listOfCmds) ->
            update firstMsg gallery
                |> Tuple.mapSecond (\cmd -> Cmd.batch [ wait (toFloat ms) (Batch listOfCmds), cmd ])

        Batch [] ->
            ( gallery, Cmd.none )

        TransitionEnd ->
            if currentSlide == List.length slides then
                update
                    (SetIndex 0)
                    gallery

            else if currentSlide == -1 then
                update (SetIndex (List.length slides - 1)) gallery

            else
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
getCurrentIndex (Gallery _ _ currentSlide _ _ _) =
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
view ((Gallery size config currentSlide dragState slides transitionSpeed) as gallery) =
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
                    ++ slidesEvents config.enableDrag dragState
                    ++ dragOffset dragState currentSlide
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
dragOffset : DragState -> CurrentSlide -> List (Attribute Msg)
dragOffset dragState currentSlide =
    let
        indexBasedOffset =
            String.fromInt ((currentSlide + 1) * -100) ++ "%"
    in
    case dragState of
        Dragging (PosX startX) (PosX currentX) ->
            [ style "left" <| "calc(" ++ indexBasedOffset ++ " - " ++ String.fromInt (startX - currentX) ++ "px)" ]

        NotDragging ->
            [ style "left" indexBasedOffset ]


{-| Create all the slider listeners required to handle the DragState |
-}
slidesEvents : Bool -> DragState -> List (Attribute Msg)
slidesEvents enableDrag dragState =
    if not enableDrag then
        []

    else
        [ on "mousedown" (Decode.map DragStart decodePosX)
        , on "touchstart" (Decode.map DragStart decodePosX)
        , on "transitionend" (Decode.succeed TransitionEnd)
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
viewStylesheet (Gallery _ config currentSlide _ slides (TransitionSpeed transitionSpeed)) =
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
                , ( "top", "0" )
                , ( "width", "" ++ String.fromInt (amountOfSlides * 100) ++ "%" )
                , ( "height", "100%" )
                , ( "display", "grid" )
                , ( "grid-template-columns", "repeat(" ++ String.fromInt amountOfSlides ++ ", 1fr)" )
                , ( "transition", "left " ++ String.fromInt transitionSpeed ++ "ms ease" )
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
