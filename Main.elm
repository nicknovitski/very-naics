module Main exposing (..)

import Html exposing (Html, button, div, p, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import NAICS exposing (NAIC, naics)
import Random exposing (Generator, int, list, minInt, maxInt)
import Regex exposing (contains, regex)
import Tuple exposing (first, second)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


intList : Int -> Generator (List Int)
intList l =
    list l (int minInt maxInt)


confidentHead : List a -> a
confidentHead l =
    let
        h =
            List.head l
    in
        case h of
            Just a ->
                a

            Nothing ->
                Debug.crash "YOU WERE WRONG"


randomFromList : List a -> Generator a
randomFromList l =
    intList (List.length l)
        |> Random.map
            ((List.map2 (,) l)
                >> (List.sortBy second)
                >> confidentHead
                >> first
            )


randomGoodNAIC : Generator NAIC
randomGoodNAIC =
    List.filter (not << (.title >> (contains (regex "Other")))) naics
        |> randomFromList


type alias Model =
    { naic : Maybe NAIC }


init : ( Model, Cmd Msg )
init =
    ( { naic = Nothing }, Random.generate NewNaic randomGoodNAIC )


type Msg
    = Roll
    | NewNaic NAIC


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            ( model, Random.generate NewNaic randomGoodNAIC )

        NewNaic n ->
            ( Model (Just n), Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div []
        (case model.naic of
            Just n ->
                [ p [class "proposal"] [ 
                    span [class "why-dont"] [ text "Why don't you specialize your services for " ]
                , span [class "industry"] [ text n.title ]
                , text "?"
                ]
                , button [ onClick Roll ] [ text "That doesn't sound right" ]
                ]
            Nothing ->
                [ button [ onClick Roll ] [ text "What industry should I specialize in?" ] ]
        )
