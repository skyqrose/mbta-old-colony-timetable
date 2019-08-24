module Main exposing (main)

import AssocList as Dict exposing (Dict)
import AssocList.Extra as Dict
import Browser
import Element as El exposing (Element)
import Http
import List.Extra
import Mbta
import Mbta.Api
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


apiHost : Mbta.Api.Host
apiHost =
    Mbta.Api.Default { apiKey = Nothing }


type alias RemoteDataApi primary =
    RemoteData.RemoteData Mbta.Api.ApiError (Mbta.Api.Data primary)


type alias Model =
    { routes : RemoteDataApi (List Mbta.Route)
    , stops : RemoteDataApi (List Mbta.Stop)
    , schedules : RemoteDataApi (List Mbta.Schedule)
    }


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
            []
            [ Mbta.Api.filterSchedulesByRouteIds routeIds
            , Mbta.Api.filterSchedulesByStopIds stopIds
            ]
        ]
    )


type Msg
    = ReceiveRoutes (Mbta.Api.ApiResult (List Mbta.Route))
    | ReceiveStops (Mbta.Api.ApiResult (List Mbta.Stop))
    | ReceiveSchedules (Mbta.Api.ApiResult (List Mbta.Schedule))


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


view : Model -> Browser.Document Msg
view model =
    { title = "MBTA Old Colony Timetable - skyqrose"
    , body =
        [ El.layout [] (body model) ]
    }


body : Model -> Element Msg
body model =
    case ( model.routes, model.stops, model.schedules ) of
        ( RemoteData.Success routes, RemoteData.Success stops, RemoteData.Success schedules ) ->
            viewData
                (Mbta.Api.getPrimaryData routes)
                (Mbta.Api.getPrimaryData stops)
                (Mbta.Api.getPrimaryData schedules)

        ( RemoteData.Loading, _, _ ) ->
            El.text "Loading"

        ( _, RemoteData.Loading, _ ) ->
            El.text "Loading"

        ( _, _, RemoteData.Loading ) ->
            El.text "Loading"

        ( RemoteData.Failure e, _, _ ) ->
            El.text (Debug.toString e)

        ( _, RemoteData.Failure e, _ ) ->
            El.text (Debug.toString e)

        ( _, _, RemoteData.Failure e ) ->
            El.text (Debug.toString e)

        _ ->
            El.text (Debug.toString model)


viewData : List Mbta.Route -> List Mbta.Stop -> List Mbta.Schedule -> Element msg
viewData routes stops schedules =
    let
        ( inboundSchedules, outboundSchedules ) =
            List.partition
                (\schedule -> schedule.directionId == Mbta.D1)
                schedules

        stopDict =
            buildParentStationDict stops
    in
    El.column
        [ El.spacing 10 ]
        [ viewTimetable stopDict inboundSchedules
        , viewTimetable stopDict outboundSchedules
        ]


viewTimetable : Dict Mbta.StopId Mbta.StopId -> List Mbta.Schedule -> Element msg
viewTimetable stopDict schedules =
    let
        trips : Dict Mbta.TripId (List Mbta.Schedule)
        trips =
            Dict.groupBy .tripId schedules
    in
    El.row
        []
        (List.map
            (\( tripId, schedulesOnTrip ) -> viewTripColumn stopDict tripId schedulesOnTrip)
            (Dict.toList trips)
        )


viewTripColumn :
    Dict Mbta.StopId Mbta.StopId
    -> Mbta.TripId
    -> List Mbta.Schedule
    -> Element msg
viewTripColumn stopDict (Mbta.TripId tripId) schedules =
    El.column
        []
        (El.text tripId
            :: (stopIds
                    |> List.map
                        (\stopId ->
                            List.Extra.find
                                (\schedule -> Dict.get schedule.stopId stopDict == Just stopId)
                                schedules
                        )
                    |> List.map
                        (\maybeSchedule ->
                            case maybeSchedule of
                                Nothing ->
                                    El.text "-"

                                Just schedule ->
                                    case schedule.id of
                                        Mbta.ScheduleId scheduleId ->
                                            El.text scheduleId
                        )
               )
        )


buildParentStationDict : List Mbta.Stop -> Dict Mbta.StopId Mbta.StopId
buildParentStationDict stops =
    stops
        |> List.concatMap childIds
        |> Dict.fromList


childIds : Mbta.Stop -> List ( Mbta.StopId, Mbta.StopId )
childIds stop =
    case stop of
        Mbta.Stop_1_Station station ->
            List.map
                (\childId -> ( childId, station.id ))
                station.childStops

        _ ->
            []


main : Program () Model Msg
main =
    Browser.document
        { init = \flags -> init
        , view = view
        , update = update
        , subscriptions = \model -> Sub.none
        }
