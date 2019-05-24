module Main exposing (main)

import Browser
import AssocList as Dict exposing (Dict)
import Http
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Mbta
import RemoteData

routeIds : List Mbta.RouteId
routeIds =
    [ Mbta.RouteId "CR-Greenbush"
    , Mbta.RouteId "CR-Middleborough"
    , Mbta.RouteId "CR-Kingston"
    ]

stopIds : List Mbta.StopId
stopIds =
    [ Mbta.StopId "place-sstat"
    , Mbta.StopId "place-jfk"
    , Mbta.StopId "place-qnctr"
    , Mbta.StopId "place-brntn"
    ]

type alias Model =
    { routes : RemoteData.WebData (List Mbta.Route)
    }


initialModel : Model
initialModel =
    { routes = RemoteData.Loading
    }


type Msg
    = ReceiveRoutes (Result Http.Error (List Mbta.Route))


update : Msg -> Model -> Model
update msg model =
    case msg of
        ReceiveRoutes routesResult ->
            { model |
                routes = RemoteData.fromResult routesResult
            }


view : Model -> Html Msg
view model =
    case model.routes of
        RemoteData.NotAsked ->
            Html.text "Not Asked"
        RemoteData.Loading ->
            Html.text "Loading"
        RemoteData.Failure e ->
            Html.text "Error"
        RemoteData.Success routes ->
            Html.div []
                (List.map (Html.text << .longName) routes)



main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
