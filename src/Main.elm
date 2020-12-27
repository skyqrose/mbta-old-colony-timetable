module Main exposing (main)

import Browser
import MakeViewModel
import Mbta
import Mbta.Api
import Model exposing (..)
import RemoteData
import View


apiHost : Mbta.Api.Host
apiHost =
    Mbta.Api.Default { apiKey = Just "84f8e2ba4820455a8b507d886863afd4" }


init : ( Model, Cmd Msg )
init =
    initCorridor NortheastCorridor


initCorridor : Corridor -> ( Model, Cmd Msg )
initCorridor corridor =
    ( { routes = RemoteData.Loading
      , stops = RemoteData.Loading
      , services = RemoteData.Loading
      , schedules = RemoteData.Loading
      , selectedCorridor = corridor
      , selectedDay = Today
      }
    , Cmd.batch
        [ Mbta.Api.getRoutes
            ReceiveRoutes
            apiHost
            []
            [ Mbta.Api.filterRoutesByIds (routeIds corridor) ]
        , Mbta.Api.getStops
            ReceiveStops
            apiHost
            [ Mbta.Api.include Mbta.Api.stopChildStops ]
            [ Mbta.Api.filterStopsByIds (stopIds corridor) ]
        , Mbta.Api.getServices
            ReceiveServices
            apiHost
            []
            [ Mbta.Api.filterServicesByRouteIds (routeIds corridor) ]
        , getSchedules corridor Today
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceiveRoutes routesResult ->
            ( { model
                | routes = RemoteData.fromResult routesResult
              }
            , Cmd.none
            )

        ReceiveStops stopsResult ->
            ( { model
                | stops = RemoteData.fromResult stopsResult
              }
            , Cmd.none
            )

        ReceiveServices servicesResult ->
            ( { model
                | services = RemoteData.fromResult servicesResult
              }
            , Cmd.none
            )

        ReceiveSchedules schedulesResult ->
            ( { model
                | schedules = RemoteData.fromResult schedulesResult
              }
            , Cmd.none
            )

        SelectCorridor corridor ->
            initCorridor corridor

        SelectDay selectedDay ->
            ( { model
                | selectedDay = selectedDay
                , schedules = RemoteData.Loading
              }
            , getSchedules model.selectedCorridor selectedDay
            )


getSchedules : Corridor -> Day -> Cmd Msg
getSchedules corridor selectedDay =
    let
        dateFilter : List (Mbta.Api.Filter Mbta.Schedule)
        dateFilter =
            case selectedDay of
                Today ->
                    [{- No date filter is implicitly today -}]

                Future serviceKey ->
                    [ Mbta.Api.filterSchedulesByServiceDate serviceKey.startDate ]
    in
    Mbta.Api.getSchedules
        ReceiveSchedules
        apiHost
        [ Mbta.Api.include Mbta.Api.scheduleTrip ]
        ([ Mbta.Api.filterSchedulesByRouteIds (routeIds corridor)
         , Mbta.Api.filterSchedulesByStopIds (stopIds corridor)
         ]
            ++ dateFilter
        )


main : Program () Model Msg
main =
    Browser.document
        { init = \flags -> init
        , view = MakeViewModel.makeViewModel >> View.view
        , update = update
        , subscriptions = \model -> Sub.none
        }
