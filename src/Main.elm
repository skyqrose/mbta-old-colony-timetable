module Main exposing (main)

import Browser
import AssocList as Dict exposing (Dict)
import Http
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Mbta
import Mbta.Api exposing (routesFilter)
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

apiConfig : Mbta.Api.Config
apiConfig =
    { host = Mbta.Api.Default
    , apiKey = Mbta.Api.NoApiKey
    }

type alias Model =
    { routes : RemoteData.WebData (List Mbta.Route)
    }


init : (Model, Cmd Msg)
init =
    ( { routes = RemoteData.Loading
      }
    , Mbta.Api.getRoutes ReceiveRoutes apiConfig { routesFilter | id = routeIds }
    )


type Msg
    = ReceiveRoutes (Result Http.Error (List Mbta.Route))


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ReceiveRoutes routesResult ->
            ( { model |
                routes = RemoteData.fromResult routesResult
              }
            , Cmd.none
            )


view : Model -> Browser.Document Msg
view model =
    { title= "MBTA Old Colony Timetable - skyqrose"
    , body =
    case model.routes of
        RemoteData.NotAsked ->
            [ Html.text "Not Asked" ]
        RemoteData.Loading ->
            [ Html.text "Loading" ]
        RemoteData.Failure e ->
            [ Html.text "Error" ]
        RemoteData.Success routes ->
            (List.map (Html.text << .longName) routes)
    }


main : Program () Model Msg
main =
    Browser.document
        { init = \flags -> init
        , view = view
        , update = update
        , subscriptions = \model -> Sub.none
        }
