module Main exposing (main)

import AssocList as Dict exposing (Dict)
import Browser
import Element as El exposing (Element)
import Http
import Mbta
import Mbta.Api
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


apiHost : Mbta.Api.Host
apiHost =
    Mbta.Api.Default { apiKey = Nothing }


type alias RemoteDataApi primary =
    RemoteData.RemoteData Mbta.Api.ApiError (Mbta.Api.Data primary)


type alias Model =
    { routes : RemoteDataApi (List Mbta.Route)
    , stops : RemoteDataApi (List Mbta.Stop)
    , schedules : RemoteDataApi (List Mbta.Schedule)
    }


init : ( Model, Cmd Msg )
init =
    ( { routes = RemoteData.Loading
      , stops = RemoteData.Loading
      , schedules = RemoteData.Loading
      }
    , Cmd.batch
        [ Mbta.Api.getRoutes
            ReceiveRoutes
            apiHost
            []
            [ Mbta.Api.filterRoutesByIds routeIds ]
        , Mbta.Api.getStops
            ReceiveStops
            apiHost
            []
            [ Mbta.Api.filterStopsByIds stopIds ]
        , Mbta.Api.getSchedules
            ReceiveSchedules
            apiHost
            []
            [ Mbta.Api.filterSchedulesByRouteIds routeIds
            , Mbta.Api.filterSchedulesByStopIds stopIds
            ]
        ]
    )


type Msg
    = ReceiveRoutes (Mbta.Api.ApiResult (List Mbta.Route))
    | ReceiveStops (Mbta.Api.ApiResult (List Mbta.Stop))
    | ReceiveSchedules (Mbta.Api.ApiResult (List Mbta.Schedule))


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
        [ El.layout [] (body model) ]
    }


body : Model -> Element Msg
body model =
    case ( model.routes, model.stops, model.schedules ) of
        ( RemoteData.Success routes, RemoteData.Success stops, RemoteData.Success schedules ) ->
            viewData
                (Mbta.Api.getPrimaryData routes)
                (Mbta.Api.getPrimaryData stops)
                (Mbta.Api.getPrimaryData schedules)

        ( RemoteData.Loading, _, _ ) ->
            El.text "Loading"

        ( _, RemoteData.Loading, _ ) ->
            El.text "Loading"

        ( _, _, RemoteData.Loading ) ->
            El.text "Loading"

        ( RemoteData.Failure e, _, _ ) ->
            El.text (Debug.toString e)

        ( _, RemoteData.Failure e, _ ) ->
            El.text (Debug.toString e)

        ( _, _, RemoteData.Failure e ) ->
            El.text (Debug.toString e)

        _ ->
            El.text (Debug.toString model)


viewData : List Mbta.Route -> List Mbta.Stop -> List Mbta.Schedule -> Element msg
viewData routes stops schedules =
    El.column
        []
        (List.map (El.text << Debug.toString) schedules)


main : Program () Model Msg
main =
    Browser.document
        { init = \flags -> init
        , view = view
        , update = update
        , subscriptions = \model -> Sub.none
        }
