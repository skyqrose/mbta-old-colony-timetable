module ViewModel exposing
    ( DayButton
    , DayButtons
    , Schedule
    , StopHeader
    , Timetable
    , Timetables
    , Trip
    , ViewModel
    )

import Mbta
import Model
import RemoteData


type alias ViewModel =
    { selectedCorridor : Model.Corridor
    , dayButtons : RemoteData.RemoteData String DayButtons
    , timetables : RemoteData.RemoteData String Timetables
    }


type alias DayButtons =
    List DayButton


type alias DayButton =
    { day : Model.Day
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
