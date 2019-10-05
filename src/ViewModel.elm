module ViewModel exposing (Schedule, StopHeader, Timetable, Timetables, Trip, ViewModel(..))

import Mbta


type ViewModel
    = LoadingServices
    | ServicesLoaded ServiceButtons
    | LoadingSchedules ServiceButtons
    | SchedulesLoaded ServiceButtons Timetables
    | Error String


type alias ServiceButtons =
    List String


type alias Timetables =
    { d0 : Timetable
    , d1 : Timetable
    }


type alias Timetable =
    { directionId : Mbta.DirectionId
    , stopHeaders : List StopHeader
    , trips : List Trip
    }


type alias StopHeader =
    { stopName : String
    , accessible : Bool
    }


type alias Trip =
    { name : Maybe String
    , route : Maybe Mbta.RouteId
    , bikes : Bool
    , schedules : List Schedule
    }


type alias Schedule =
    Maybe String
