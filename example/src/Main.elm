module Main exposing (main)

import Browser exposing (element)
import Browser.Events exposing (onKeyDown)
import Html exposing (Html, a, button, div, h1, map, p, text)
import Html.Attributes exposing (attribute, href)
import Html.Events exposing (onClick)
import InfiniteGallery as Gallery
import Json.Decode as Decode


type alias Model =
    { gallery : Gallery.Gallery }


type Msg
    = MsgForGallery Gallery.Msg
    | Previous
    | Next


main : Program () Model Msg
main =
    element
        { init = init
        , update = update
        , subscriptions = always (onKeyDown makeArrowsScroll)
        , view = view
        }


makeArrowsScroll : Decode.Decoder Msg
makeArrowsScroll =
    Decode.field "key" Decode.string
        |> Decode.andThen arrowsToSlideNavigation


arrowsToSlideNavigation : String -> Decode.Decoder Msg
arrowsToSlideNavigation keyString =
    case keyString of
        "ArrowLeft" ->
            Decode.succeed Previous

        "ArrowRight" ->
            Decode.succeed Next

        _ ->
            Decode.fail ""


init : () -> ( Model, Cmd Msg )
init _ =
    let
        size =
            { width = "auto", height = "500px" }

        config =
            Gallery.defaultConfig
    in
    ( List.map viewSlide (List.range 0 4)
        |> Gallery.init size config
        |> Model
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg { gallery } =
    case msg of
        MsgForGallery galleryMsg ->
            Gallery.update galleryMsg gallery
                |> Tuple.mapBoth
                    (\a -> { gallery = a })
                    (Cmd.map MsgForGallery)

        Previous ->
            Gallery.previous gallery
                |> Tuple.mapBoth
                    (\a -> { gallery = a })
                    (Cmd.map MsgForGallery)

        Next ->
            Gallery.next gallery
                |> Tuple.mapBoth
                    (\a -> { gallery = a })
                    (Cmd.map MsgForGallery)


view : Model -> Html Msg
view { gallery } =
    div []
        [ h1 [] [ text "Elm Infinite Gallery Example" ]
        , p []
            [ a [ href "https://github.com/bellroy/elm-infinite-gallery" ] [ text "view source" ]
            , text " "
            , a [ href "https://package.elm-lang.org/packages/bellroy/elm-infinite-gallery/latest/" ] [ text "view documentation" ]
            ]
        , Gallery.view gallery
            |> Html.map MsgForGallery
        , button
            [ onClick Previous ]
            [ text "<<" ]
        , button
            [ onClick Next ]
            [ text ">>" ]
        ]


viewSlide : Int -> Html msg
viewSlide index =
    let
        color =
            case index of
                0 ->
                    "#666"

                1 ->
                    "#555"

                2 ->
                    "#444"

                3 ->
                    "#333"

                _ ->
                    "#222"
    in
    div [ attribute "style" ("width: 100%; height: 100%; background-color: " ++ color ++ "; text-align: center; display: flex; align-items: center; justify-content: center;") ]
        [ div [ attribute "style" "color: #fff; font-family: monospace; font-size: 40px; font-weight: bold;" ]
            [ text (String.fromInt index) ]
        ]
