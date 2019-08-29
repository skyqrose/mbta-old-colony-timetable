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
import Time
import TimeZone


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


timeZone : Time.Zone
timeZone =
    TimeZone.america__new_york ()


noStopIndication : Element msg
noStopIndication =
    El.text "-"


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
            [ Mbta.Api.include Mbta.Api.scheduleTrip ]
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
                (\tripId -> Mbta.Api.getIncludedTrip tripId schedules)

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


viewData : List Mbta.Route -> List Mbta.Stop -> List Mbta.Schedule -> (Mbta.TripId -> Maybe Mbta.Trip) -> Element msg
viewData routes stops schedules tripGetter =
    let
        ( inboundSchedules, outboundSchedules ) =
            List.partition
                (\schedule -> schedule.directionId == Mbta.D1)
                schedules

        stopDict =
            buildParentStationDict stops

        sortedStops =
            List.filterMap
                (\stopId ->
                    List.Extra.find (\stop -> Mbta.stopId stop == stopId) stops
                )
                stopIds
    in
    El.column
        [ El.spacing 10 ]
        [ viewTimetable sortedStops stopDict inboundSchedules tripGetter
        , viewTimetable sortedStops stopDict outboundSchedules tripGetter
        ]


viewTimetable : List Mbta.Stop -> Dict Mbta.StopId Mbta.StopId -> List Mbta.Schedule -> (Mbta.TripId -> Maybe Mbta.Trip) -> Element msg
viewTimetable stops stopDict schedules tripGetter =
    let
        trips : Dict Mbta.TripId (List Mbta.Schedule)
        trips =
            Dict.groupBy .tripId schedules
    in
    El.row
        []
        (viewStopHeader stops
            :: (trips
                    |> Dict.toList
                    |> List.sortBy
                        (\( tripId, schedulesOnTrip ) ->
                            schedulesOnTrip
                                |> List.Extra.find
                                    (\schedule ->
                                        Dict.get schedule.stopId stopDict == Just (Mbta.StopId "place-sstat")
                                    )
                                |> Maybe.andThen scheduleToTime
                                |> Maybe.map Time.posixToMillis
                                |> Maybe.withDefault 0
                        )
                    |> List.map
                        (\( tripId, schedulesOnTrip ) -> viewTripColumn stopDict (tripGetter tripId) schedulesOnTrip)
               )
        )


viewStopHeader : List Mbta.Stop -> Element msg
viewStopHeader stops =
    El.column
        []
        (List.concat
            [ [ El.text "" ]
            , List.map viewStopHeaderCell stops
            , [ El.text "" ]
            ]
        )


viewStopHeaderCell : Mbta.Stop -> Element msg
viewStopHeaderCell stop =
    El.column
        []
        [ El.text (Mbta.stopName stop)
        , if Mbta.stopWheelchairAccessible stop == Mbta.Accessible_1_Accessible then
            El.text "accessible"

          else
            El.text " "
        ]


viewTripColumn :
    Dict Mbta.StopId Mbta.StopId
    -> Maybe Mbta.Trip
    -> List Mbta.Schedule
    -> Element msg
viewTripColumn stopDict trip schedules =
    El.column
        []
        (tripDescriptor trip
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
                                    noStopIndication

                                Just schedule ->
                                    case viewScheduleTime schedule of
                                        Just timeString ->
                                            El.text timeString

                                        Nothing ->
                                            noStopIndication
                        )
               )
        )


tripDescriptor : Maybe Mbta.Trip -> Element msg
tripDescriptor maybeTrip =
    case maybeTrip of
        Nothing ->
            El.text ""

        Just trip ->
            El.text trip.name


scheduleToTime : Mbta.Schedule -> Maybe Time.Posix
scheduleToTime schedule =
    case ( schedule.arrivalTime, schedule.departureTime ) of
        ( _, Just departureTime ) ->
            Just departureTime

        ( Just arrivalTime, _ ) ->
            Just arrivalTime

        ( Nothing, Nothing ) ->
            Nothing


viewScheduleTime : Mbta.Schedule -> Maybe String
viewScheduleTime schedule =
    schedule
        |> scheduleToTime
        |> Maybe.map
            (\time ->
                String.concat
                    [ String.fromInt (Time.toHour timeZone time)
                    , ":"
                    , String.fromInt (Time.toMinute timeZone time)
                    ]
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
