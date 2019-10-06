module View exposing (view)

import Browser
import Element as El exposing (Element, rgb)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Mbta
import Model exposing (Msg)
import ViewModel


scheduleCellStyling : List (El.Attribute msg)
scheduleCellStyling =
    [ El.height (El.px 60)
    , El.padding 5
    ]


shadedColor : El.Color
shadedColor =
    rgb 0.95 0.95 0.95


evenRowStyling : List (El.Attribute msg)
evenRowStyling =
    [ Background.color (rgb 1.0 1.0 1.0) ]


oddRowStyling : List (El.Attribute msg)
oddRowStyling =
    [ Background.color shadedColor ]


view : ViewModel.ViewModel -> Browser.Document Msg
view model =
    { title = "MBTA Old Colony Timetable - skyqrose"
    , body =
        [ El.layout [] (body model) ]
    }


body : ViewModel.ViewModel -> Element Msg
body model =
    case model of
        ViewModel.LoadingServices ->
            El.text "Loading services"

        ViewModel.ServicesLoaded dayButtons ->
            El.column
                []
                [ viewDayButtons dayButtons ]

        ViewModel.LoadingSchedules dayButtons ->
            El.column
                []
                [ viewDayButtons dayButtons
                , El.text "Loading"
                ]

        ViewModel.SchedulesLoaded dayButtons timetables ->
            El.column
                []
                [ viewDayButtons dayButtons
                , viewTimetables timetables
                ]

        ViewModel.Error e ->
            El.text e


viewDayButtons : ViewModel.DayButtons -> Element Msg
viewDayButtons dayButtons =
    El.row
        []
        (List.map
            (\dayButton ->
                Input.button
                    ([ El.padding 5
                     , Border.width 1
                     , Border.rounded 10
                     ]
                        ++ (if dayButton.isSelected then
                                [ Background.color shadedColor ]

                            else
                                []
                           )
                    )
                    { onPress = Just (Model.SelectDay dayButton.day)
                    , label = El.text dayButton.text
                    }
            )
            dayButtons
        )


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
        [ directionHeading
            (case timetable.directionId of
                Mbta.D1 ->
                    "Inbound"

                Mbta.D0 ->
                    "Outbound"
            )
        , viewTable timetable
        ]


viewTable : ViewModel.Timetable -> Element msg
viewTable timetable =
    El.row
        [ El.padding 10
        ]
        (viewStopHeaders timetable.directionId timetable.stopHeaders :: List.map viewTripColumn timetable.trips)


viewStopHeaders : Mbta.DirectionId -> List ViewModel.StopHeader -> Element msg
viewStopHeaders directionId stopHeaders =
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
            , List.indexedMap viewStopHeaderCell stopHeaders
            , [ El.el
                    (scheduleCellStyling ++ [ Font.variant Font.smallCaps ])
                    (El.text
                        (case directionId of
                            Mbta.D0 ->
                                "To"

                            Mbta.D1 ->
                                "From"
                        )
                    )
              ]
            ]
        )


viewStopHeaderCell : Int -> ViewModel.StopHeader -> Element msg
viewStopHeaderCell i stopHeader =
    El.column
        ((if modBy 2 i == 0 then
            evenRowStyling

          else
            oddRowStyling
         )
            ++ [ El.width El.fill ]
            ++ scheduleCellStyling
        )
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
            , List.indexedMap viewSchedule trip.schedules
            , [ tripFooter trip ]
            ]
        )


viewSchedule : Int -> ViewModel.Schedule -> Element msg
viewSchedule i schedule =
    schedule
        |> Maybe.withDefault "-"
        |> El.text
        |> El.el
            ((if modBy 2 i == 0 then
                evenRowStyling

              else
                oddRowStyling
             )
                ++ [ El.width El.fill ]
                ++ scheduleCellStyling
            )


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
