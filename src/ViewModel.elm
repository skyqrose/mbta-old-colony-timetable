module ViewModel exposing
    ( Schedule
    , ServiceButton
    , ServiceButtons
    , StopHeader
    , Timetable
    , Timetables
    , Trip
    , ViewModel(..)
    )

import Mbta
import Model


type ViewModel
    = LoadingServices
    | ServicesLoaded ServiceButtons
    | LoadingSchedules ServiceButtons
    | SchedulesLoaded ServiceButtons Timetables
    | Error String


type alias ServiceButtons =
    List ServiceButton


type alias ServiceButton =
    { serviceKey : Model.ServiceKey
    , text : String
    , isSelected : Bool
    }


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
