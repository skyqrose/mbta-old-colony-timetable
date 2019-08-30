module MakeViewModel exposing (makeViewModel)

import AssocList as Dict exposing (Dict)
import AssocList.Extra as Dict
import List.Extra
import Mbta
import Mbta.Api
import Model
import RemoteData
import Time
import TimeZone
import ViewModel


makeViewModel : Model.Model -> ViewModel.ViewModel
makeViewModel model =
    case ( model.routes, model.stops, model.schedules ) of
        ( RemoteData.Success routes, RemoteData.Success stops, RemoteData.Success schedules ) ->
            ViewModel.Success
                (viewTimetables
                    (Mbta.Api.getPrimaryData routes)
                    (Mbta.Api.getPrimaryData stops)
                    (Mbta.Api.getPrimaryData schedules)
                    (\tripId -> Mbta.Api.getIncludedTrip tripId schedules)
                )

        ( RemoteData.Loading, _, _ ) ->
            ViewModel.Loading

        ( _, RemoteData.Loading, _ ) ->
            ViewModel.Loading

        ( _, _, RemoteData.Loading ) ->
            ViewModel.Loading

        ( RemoteData.Failure e, _, _ ) ->
            ViewModel.Error (Debug.toString e)

        ( _, RemoteData.Failure e, _ ) ->
            ViewModel.Error (Debug.toString e)

        ( _, _, RemoteData.Failure e ) ->
            ViewModel.Error (Debug.toString e)

        _ ->
            ViewModel.Error (Debug.toString model)


viewTimetables :
    List Mbta.Route
    -> List Mbta.Stop
    -> List Mbta.Schedule
    -> (Mbta.TripId -> Maybe Mbta.Trip)
    -> ViewModel.Timetables
viewTimetables routes stops schedules tripGetter =
    let
        ( inboundSchedules, outboundSchedules ) =
            List.partition
                (\schedule -> schedule.directionId == Mbta.D1)
                schedules

        stopDict =
            buildParentStationDict stops
    in
    { d0 = viewTimetable stops stopDict outboundSchedules tripGetter
    , d1 = viewTimetable stops stopDict inboundSchedules tripGetter
    }


viewTimetable : List Mbta.Stop -> Dict Mbta.StopId Mbta.StopId -> List Mbta.Schedule -> (Mbta.TripId -> Maybe Mbta.Trip) -> ViewModel.Timetable
viewTimetable stops stopDict schedules tripGetter =
    let
        trips : Dict Mbta.TripId (List Mbta.Schedule)
        trips =
            Dict.groupBy .tripId schedules
    in
    { stopHeaders = viewStopHeaders stops
    , trips =
        trips
            |> Dict.toList
            |> List.sortBy
                (\( tripId, schedulesOnTrip ) ->
                    schedulesOnTrip
                        |> List.Extra.find
                            (\schedule ->
                                Dict.get schedule.stopId stopDict
                                    == Just (Mbta.StopId "place-sstat")
                            )
                        |> Maybe.andThen scheduleToTime
                        |> Maybe.map Time.posixToMillis
                        |> Maybe.withDefault 0
                )
            |> List.map
                (\( tripId, schedulesOnTrip ) ->
                    viewTrip stopDict (tripGetter tripId) schedulesOnTrip
                )
    }


viewStopHeaders : List Mbta.Stop -> List ViewModel.StopHeader
viewStopHeaders stops =
    let
        sortedStops =
            List.filterMap
                (\stopId ->
                    List.Extra.find (\stop -> Mbta.stopId stop == stopId) stops
                )
                Model.stopIds
    in
    List.map
        (\stop ->
            { stopName = Mbta.stopName stop
            , accessible = Mbta.stopWheelchairAccessible stop == Mbta.Accessible_1_Accessible
            }
        )
        sortedStops


viewTrip :
    Dict Mbta.StopId Mbta.StopId
    -> Maybe Mbta.Trip
    -> List Mbta.Schedule
    -> ViewModel.Trip
viewTrip stopDict maybeTrip schedules =
    { name = Maybe.map .name maybeTrip
    , bikes =
        case maybeTrip of
            Just trip ->
                trip.bikesAllowed == Mbta.Bikes_1_Allowed

            Nothing ->
                False
    , schedules =
        Model.stopIds
            |> List.map
                (\stopId ->
                    List.Extra.find
                        (\schedule -> Dict.get schedule.stopId stopDict == Just stopId)
                        schedules
                )
            |> List.map (Maybe.andThen viewScheduleTime)
    }


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
                    [ time |> Time.toHour timeZone |> String.fromInt |> String.padLeft 2 '0'
                    , ":"
                    , time |> Time.toMinute timeZone |> String.fromInt |> String.padLeft 2 '0'
                    ]
            )


timeZone : Time.Zone
timeZone =
    TimeZone.america__new_york ()


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
