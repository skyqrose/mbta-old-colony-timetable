module Main exposing (main)

import AssocList as Dict exposing (Dict)
import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Http
import Mbta
import Mbta.Api exposing (routesFilter, schedulesFilter, stopsFilter)
import RemoteData


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


apiConfig : Mbta.Api.Config
apiConfig =
    { host = Mbta.Api.Default
    , apiKey = Mbta.Api.NoApiKey
    }


type alias Model =
    { routes : RemoteData.WebData (List Mbta.Route)
    , stops : RemoteData.WebData (List Mbta.Stop)
    , schedules : RemoteData.WebData (List Mbta.Schedule)
    }


init : ( Model, Cmd Msg )
init =
    ( { routes = RemoteData.Loading
      , stops = RemoteData.Loading
      , schedules = RemoteData.Loading
      }
    , Cmd.batch
        [ Mbta.Api.getRoutes ReceiveRoutes apiConfig { routesFilter | id = routeIds }
        , Mbta.Api.getStops ReceiveStops apiConfig { stopsFilter | id = stopIds }
        , Mbta.Api.getSchedules ReceiveSchedules
            apiConfig
            { schedulesFilter
                | route = routeIds
                , stop = stopIds
            }
        ]
    )


type Msg
    = ReceiveRoutes (Result Http.Error (List Mbta.Route))
    | ReceiveStops (Result Http.Error (List Mbta.Stop))
    | ReceiveSchedules (Result Http.Error (List Mbta.Schedule))


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
        (case model.routes of
            RemoteData.NotAsked ->
                [ Html.text "Not Asked" ]

            RemoteData.Loading ->
                [ Html.text "Loading" ]

            RemoteData.Failure e ->
                [ Html.text "Error" ]

            RemoteData.Success routes ->
                List.map (Html.text << .longName) routes
        )
            ++ (case model.stops of
                    RemoteData.NotAsked ->
                        [ Html.text "Not Asked" ]

                    RemoteData.Loading ->
                        [ Html.text "Loading" ]

                    RemoteData.Failure e ->
                        [ Html.text "Error" ]

                    RemoteData.Success stops ->
                        List.map (Html.text << .name) stops
               )
    }


main : Program () Model Msg
main =
    Browser.document
        { init = \flags -> init
        , view = view
        , update = update
        , subscriptions = \model -> Sub.none
        }
