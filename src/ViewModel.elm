module ViewModel exposing (Schedule, StopHeader, Timetable, Timetables, Trip, ViewModel(..))


type ViewModel
    = Loading
    | Error String
    | Success Timetables


type alias Timetables =
    { d0 : Timetable
    , d1 : Timetable
    }


type alias Timetable =
    { stopHeaders : List StopHeader
    , trips : List Trip
    }


type alias StopHeader =
    { stopName : String
    , accessible : Bool
    }


type alias Trip =
    { name : Maybe String
    , route : Maybe String
    , bikes : Bool
    , schedules : List Schedule
    }


type alias Schedule =
    Maybe String
