module Main exposing (main)

import Browser
import MakeViewModel
import Mbta.Api
import Model exposing (..)
import RemoteData
import View


apiHost : Mbta.Api.Host
apiHost =
    Mbta.Api.Default { apiKey = Just "84f8e2ba4820455a8b507d886863afd4" }


init : ( Model, Cmd Msg )
init =
    ( { routes = RemoteData.Loading
      , stops = RemoteData.Loading
      , schedules = RemoteData.Loading
      }
    , Cmd.batch
        [ Mbta.Api.getRoutes
            ReceiveRoutes
            apiHost
            []
            [ Mbta.Api.filterRoutesByIds routeIds ]
        , Mbta.Api.getStops
            ReceiveStops
            apiHost
            [ Mbta.Api.include Mbta.Api.stopChildStops ]
            [ Mbta.Api.filterStopsByIds stopIds ]
        , Mbta.Api.getSchedules
            ReceiveSchedules
            apiHost
            [ Mbta.Api.include Mbta.Api.scheduleTrip ]
            [ Mbta.Api.filterSchedulesByRouteIds routeIds
            , Mbta.Api.filterSchedulesByStopIds stopIds
            ]
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

        ReceiveSchedules schedulesResult ->
            ( { model
                | schedules = RemoteData.fromResult schedulesResult
              }
            , Cmd.none
            )


main : Program () Model Msg
main =
    Browser.document
        { init = \flags -> init
        , view = MakeViewModel.makeViewModel >> View.view
        , update = update
        , subscriptions = \model -> Sub.none
        }
