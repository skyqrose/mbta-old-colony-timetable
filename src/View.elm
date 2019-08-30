module View exposing (view)

import Browser
import Element as El exposing (Element)
import Model exposing (Msg)
import ViewModel


scheduleCellStyling : List (El.Attribute msg)
scheduleCellStyling =
    [ El.height (El.px 60) ]


view : ViewModel.ViewModel -> Browser.Document Msg
view model =
    { title = "MBTA Old Colony Timetable - skyqrose"
    , body =
        [ El.layout [] (body model) ]
    }


body : ViewModel.ViewModel -> Element Msg
body model =
    case model of
        ViewModel.Loading ->
            El.text "Loading"

        ViewModel.Error e ->
            El.text e

        ViewModel.Success timetables ->
            viewTimetables timetables


viewTimetables : ViewModel.Timetables -> Element msg
viewTimetables timetables =
    El.column
        [ El.spacing 10 ]
        [ viewTimetable timetables.d1
        , viewTimetable timetables.d0
        ]


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
    , schedules : List Schedule
    }


type alias Schedule =
    Maybe String


viewTimetable : ViewModel.Timetable -> Element msg
viewTimetable timetable =
    El.row
        [ El.spacing 15 ]
        (viewStopHeaders timetable.stopHeaders :: List.map viewTripColumn timetable.trips)


viewStopHeaders : List ViewModel.StopHeader -> Element msg
viewStopHeaders stopHeaders =
    El.column
        []
        (List.concat
            [ [ El.text "" ]
            , List.map viewStopHeaderCell stopHeaders
            ]
        )


viewStopHeaderCell : ViewModel.StopHeader -> Element msg
viewStopHeaderCell stopHeader =
    El.column
        scheduleCellStyling
        [ El.text stopHeader.stopName
        , if stopHeader.accessible then
            El.text "accessible"

          else
            El.text " "
        ]


viewTripColumn : ViewModel.Trip -> Element msg
viewTripColumn trip =
    El.column
        []
        (tripDescriptor trip
            :: List.map
                viewSchedule
                trip.schedules
        )


viewSchedule : ViewModel.Schedule -> Element msg
viewSchedule schedule =
    schedule
        |> Maybe.withDefault "-"
        |> El.text
        |> El.el scheduleCellStyling


tripDescriptor : ViewModel.Trip -> Element msg
tripDescriptor trip =
    El.column
        scheduleCellStyling
        [ case trip.name of
            Nothing ->
                El.text "-"

            Just name ->
                El.text name
        , if trip.bikes then
            El.text "bikes"

          else
            El.text " "
        ]
