module View exposing (view)

import Browser
import Element as El exposing (Element)
import Element.Border as Border
import Element.Font as Font
import Element.Region as Region
import Mbta
import Model exposing (Msg)
import ViewModel


scheduleCellStyling : List (El.Attribute msg)
scheduleCellStyling =
    [ El.height (El.px 60)
    , El.padding 5
    ]


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
        [ El.padding 10
        , El.spacing 20
        ]
        [ viewTimetable timetables.d1
        , viewTimetable timetables.d0
        ]


directionHeading : String -> Element msg
directionHeading text =
    El.el
        [ Region.heading 2
        , El.centerX
        , Font.size 24
        ]
        (El.text text)


viewTimetable : ViewModel.Timetable -> Element msg
viewTimetable timetable =
    El.column
        [ El.spacing 10 ]
        [ directionHeading "Inbound"
        , viewTable timetable
        ]


viewTable : ViewModel.Timetable -> Element msg
viewTable timetable =
    El.row
        [ El.padding 10
        ]
        (viewStopHeaders timetable.stopHeaders :: List.map viewTripColumn timetable.trips)


viewStopHeaders : List ViewModel.StopHeader -> Element msg
viewStopHeaders stopHeaders =
    El.column
        []
        (List.concat
            [ [ El.el
                    (scheduleCellStyling
                        ++ [ El.width El.fill
                           , Border.widthEach
                                { bottom = 1
                                , left = 0
                                , right = 0
                                , top = 0
                                }
                           ]
                    )
                    (El.text "")
              ]
            , List.map viewStopHeaderCell stopHeaders
            , [ El.el scheduleCellStyling (El.text "") ]
            ]
        )


viewStopHeaderCell : ViewModel.StopHeader -> Element msg
viewStopHeaderCell stopHeader =
    El.column
        scheduleCellStyling
        [ El.text stopHeader.stopName
        , if stopHeader.accessible then
            El.image
                [ El.height (El.px 12) ]
                { src = "/assets/accessible.svg"
                , description = "accessible"
                }

          else
            El.text " "
        ]


viewTripColumn : ViewModel.Trip -> Element msg
viewTripColumn trip =
    El.column
        [ Border.widthEach
            { bottom = 0
            , left = 1
            , right = 0
            , top = 0
            }
        ]
        (List.concat
            [ [ tripDescriptor trip ]
            , List.map viewSchedule trip.schedules
            , [ tripFooter trip ]
            ]
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
        (scheduleCellStyling
            ++ [ El.width El.fill
               , Border.widthEach
                    { bottom = 1
                    , left = 0
                    , right = 0
                    , top = 0
                    }
               ]
        )
        [ case trip.name of
            Nothing ->
                El.text "-"

            Just name ->
                El.text name
        , if trip.bikes then
            El.image
                [ El.height (El.px 20)
                , El.centerX
                ]
                { src = "/assets/bike.svg"
                , description = "bikes allowed"
                }

          else
            El.text " "
        ]


tripFooter : ViewModel.Trip -> Element msg
tripFooter trip =
    (case trip.route of
        Just (Mbta.RouteId "CR-Middleborough") ->
            "MID"

        Just (Mbta.RouteId "CR-Kingston") ->
            "KIN"

        Just (Mbta.RouteId "CR-Greenbush") ->
            "GRN"

        Just _ ->
            ""

        Nothing ->
            ""
    )
        |> El.text
        |> El.el (scheduleCellStyling ++ [ Font.variant Font.smallCaps ])
