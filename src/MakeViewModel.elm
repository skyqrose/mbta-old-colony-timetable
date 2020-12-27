module MakeViewModel exposing (makeViewModel)

import AssocList as Dict exposing (Dict)
import AssocList.Extra as Dict
import Helpers
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
    { selectedCorridor = model.selectedCorridor
    , dayButtons =
        RemoteData.mapBoth
            (\services ->
                viewDayButtons
                    (Mbta.Api.getPrimaryData services)
                    model.selectedDay
            )
            (\e -> Debug.toString e)
            model.services
    , timetables =
        RemoteData.succeed (\routes stops schedules -> ( routes, stops, schedules ))
            |> RemoteData.andMap model.routes
            |> RemoteData.andMap model.stops
            |> RemoteData.andMap model.schedules
            |> RemoteData.mapBoth
                (\( routes, stops, schedules ) ->
                    viewTimetables
                        model.selectedCorridor
                        (Mbta.Api.getPrimaryData routes)
                        (Mbta.Api.getPrimaryData stops)
                        (Mbta.Api.getPrimaryData schedules)
                        (\tripId -> Mbta.Api.getIncludedTrip tripId schedules)
                )
                (\e -> Debug.toString e)
    }


viewDayButtons :
    List Mbta.Service
    -> Model.Day
    -> ViewModel.DayButtons
viewDayButtons services selectedDay =
    let
        serviceKeys : List Model.ServiceKey
        serviceKeys =
            services
                |> List.map Model.serviceKey
                |> Helpers.uniq
                |> sortServiceKeys

        days : List Model.Day
        days =
            Model.Today :: List.map Model.Future serviceKeys
    in
    List.map (viewDayButton selectedDay) days


{-| Sorts to Weekday, Saturday, Sunday, others
ties are broken by start date
-}
sortServiceKeys : List Model.ServiceKey -> List Model.ServiceKey
sortServiceKeys serviceKeys =
    List.sortBy
        (\serviceKey ->
            ( case serviceKey.name of
                Just "Weekday" ->
                    0

                Just "Saturday" ->
                    1

                Just "Sunday" ->
                    2

                _ ->
                    3
            , Mbta.serviceDateToIso8601 serviceKey.startDate
            )
        )
        serviceKeys


viewDayButton : Model.Day -> Model.Day -> ViewModel.DayButton
viewDayButton selectedDay buttonDay =
    { day = buttonDay
    , text =
        case buttonDay of
            Model.Today ->
                "Today"

            Model.Future serviceKey ->
                serviceKey.name
                    |> Maybe.withDefault "Service"
    , isSelected = selectedDay == buttonDay
    }


viewTimetables :
    Model.Corridor
    -> List Mbta.Route
    -> List Mbta.Stop
    -> List Mbta.Schedule
    -> (Mbta.TripId -> Maybe Mbta.Trip)
    -> ViewModel.Timetables
viewTimetables corridor routes stops schedules tripGetter =
    let
        ( inboundSchedules, outboundSchedules ) =
            List.partition
                (\schedule -> schedule.directionId == Mbta.D1)
                schedules

        stopDict =
            buildParentStationDict stops
    in
    { d0 = viewTimetable corridor Mbta.D0 stops stopDict outboundSchedules tripGetter
    , d1 = viewTimetable corridor Mbta.D1 stops stopDict inboundSchedules tripGetter
    }


viewTimetable : Model.Corridor -> Mbta.DirectionId -> List Mbta.Stop -> Dict Mbta.StopId Mbta.StopId -> List Mbta.Schedule -> (Mbta.TripId -> Maybe Mbta.Trip) -> ViewModel.Timetable
viewTimetable corridor directionId stops stopDict schedules tripGetter =
    let
        trips : Dict Mbta.TripId (List Mbta.Schedule)
        trips =
            Dict.groupBy .tripId schedules
    in
    { directionId = directionId
    , stopHeaders = viewStopHeaders corridor stops
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
                    viewTrip corridor stopDict (tripGetter tripId) schedulesOnTrip
                )
    }


viewStopHeaders : Model.Corridor -> List Mbta.Stop -> List ViewModel.StopHeader
viewStopHeaders corridor stops =
    let
        sortedStops =
            List.filterMap
                (\stopId ->
                    List.Extra.find (\stop -> Mbta.stopId stop == stopId) stops
                )
                (Model.stopIds corridor)
    in
    List.map
        (\stop ->
            { stopName = Mbta.stopName stop
            , accessible = Mbta.stopWheelchairAccessible stop == Mbta.Accessible_1_Accessible
            }
        )
        sortedStops


viewTrip :
    Model.Corridor
    -> Dict Mbta.StopId Mbta.StopId
    -> Maybe Mbta.Trip
    -> List Mbta.Schedule
    -> ViewModel.Trip
viewTrip corridor stopDict maybeTrip schedules =
    { name = Maybe.andThen .name maybeTrip
    , route = Maybe.map .routeId maybeTrip
    , bikes =
        case maybeTrip of
            Just trip ->
                trip.bikesAllowed == Mbta.Bikes_1_Allowed

            Nothing ->
                False
    , schedules =
        Model.stopIds corridor
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
