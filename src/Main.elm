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
    ( { routes = RemoteData.Loading
      , stops = RemoteData.Loading
      , services = RemoteData.Loading
      , schedules = RemoteData.NotAsked
      , selectedServiceKey = Nothing
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
        , Mbta.Api.getServices
            ReceiveServices
            apiHost
            []
            [ Mbta.Api.filterServicesByRouteIds routeIds ]
        , Mbta.Api.getSchedules
            ReceiveSchedules
            apiHost
            [ Mbta.Api.include
                (Mbta.Api.scheduleTrip
                    -- Include the service so we can use it to select an initial serviceKey
                    |> Mbta.Api.andIts Mbta.Api.tripService
                )
            ]
            [ Mbta.Api.filterSchedulesByRouteIds routeIds
            , Mbta.Api.filterSchedulesByStopIds stopIds

            -- Implicilty filters to today's schedule
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

        ReceiveServices servicesResult ->
            ( { model
                | services = RemoteData.fromResult servicesResult
              }
            , Cmd.none
            )

        ReceiveSchedules schedulesResult ->
            ( { model
                | schedules = RemoteData.fromResult schedulesResult
                , selectedServiceKey =
                    -- If we haven't selected any services (like for the first fetch), select the one for the day we just got
                    case model.selectedServiceKey of
                        Nothing ->
                            serviceKeyFromSchedulesResult schedulesResult

                        Just _ ->
                            model.selectedServiceKey
              }
            , Cmd.none
            )

        SelectServiceKey serviceKey ->
            ( { model
                | selectedServiceKey = Just serviceKey
              }
            , Mbta.Api.getSchedules
                ReceiveSchedules
                apiHost
                [ Mbta.Api.include Mbta.Api.scheduleTrip ]
                [ Mbta.Api.filterSchedulesByRouteIds routeIds
                , Mbta.Api.filterSchedulesByStopIds stopIds
                , Mbta.Api.filterSchedulesByServiceDate serviceKey.startDate
                ]
            )


{-| Requires trips and services to be included
-}
serviceKeyFromSchedulesResult : Mbta.Api.ApiResult (List Mbta.Schedule) -> Maybe ServiceKey
serviceKeyFromSchedulesResult schedulesResult =
    schedulesResult
        |> Result.toMaybe
        |> Maybe.andThen
            (\data ->
                let
                    schedules =
                        Mbta.Api.getPrimaryData data
                in
                schedules
                    |> List.head
                    |> Maybe.map .tripId
                    |> Maybe.andThen (\tripId -> Mbta.Api.getIncludedTrip tripId data)
                    |> Maybe.map .serviceId
                    |> Maybe.andThen (\serviceId -> Mbta.Api.getIncludedService serviceId data)
                    |> Maybe.map Model.serviceKey
            )


main : Program () Model Msg
main =
    Browser.document
        { init = \flags -> init
        , view = MakeViewModel.makeViewModel >> View.view
        , update = update
        , subscriptions = \model -> Sub.none
        }
