module Model exposing
    ( Corridor(..)
    , Day(..)
    , Model
    , Msg(..)
    , ServiceKey
    , routeIds
    , serviceKey
    , stopIds
    )

import Mbta
import Mbta.Api
import RemoteData


type alias Model =
    { routes : RemoteDataApi (List Mbta.Route)
    , stops : RemoteDataApi (List Mbta.Stop)
    , services : RemoteDataApi (List Mbta.Service)
    , schedules : RemoteDataApi (List Mbta.Schedule)
    , selectedCorridor : Corridor
    , selectedDay : Day
    }


type Msg
    = ReceiveRoutes (Mbta.Api.ApiResult (List Mbta.Route))
    | ReceiveStops (Mbta.Api.ApiResult (List Mbta.Stop))
    | ReceiveServices (Mbta.Api.ApiResult (List Mbta.Service))
    | ReceiveSchedules (Mbta.Api.ApiResult (List Mbta.Schedule))
    | SelectCorridor Corridor
    | SelectDay Day


type alias RemoteDataApi primary =
    RemoteData.RemoteData Mbta.Api.ApiError (Mbta.Api.Data primary)


type Corridor
    = NortheastCorridor
    | OldColony


routeIds : Corridor -> List Mbta.RouteId
routeIds corridor =
    case corridor of
        NortheastCorridor ->
            [ Mbta.RouteId "CR-Worcester"
            , Mbta.RouteId "CR-Needham"
            , Mbta.RouteId "CR-Franklin"
            , Mbta.RouteId "CR-Fairmount"
            , Mbta.RouteId "CR-Foxboro"
            , Mbta.RouteId "CR-Providence"
            ]

        OldColony ->
            [ Mbta.RouteId "CR-Greenbush"
            , Mbta.RouteId "CR-Middleborough"
            , Mbta.RouteId "CR-Kingston"
            ]


stopIds : Corridor -> List Mbta.StopId
stopIds corridor =
    case corridor of
        NortheastCorridor ->
            [ Mbta.StopId "place-sstat"
            , Mbta.StopId "place-bbsta"
            , Mbta.StopId "place-rugg"
            , Mbta.StopId "place-forhl"
            , Mbta.StopId "place-NEC-2203"
            , Mbta.StopId "place-DB-0095"
            ]

        OldColony ->
            [ Mbta.StopId "place-sstat"
            , Mbta.StopId "place-jfk"
            , Mbta.StopId "place-qnctr"
            , Mbta.StopId "place-brntn"
            ]


type Day
    = Today
    | Future ServiceKey


{-| Two services are the same, running at the same time, if all the fields except the id match
-}
type alias ServiceKey =
    { description : Maybe String
    , serviceType : Maybe Mbta.ServiceType
    , name : Maybe String
    , typicality : Mbta.ServiceTypicality
    , startDate : Mbta.ServiceDate
    , endDate : Mbta.ServiceDate
    , validDays : List Int
    , addedDates : List Mbta.ChangedDate
    , removedDates : List Mbta.ChangedDate
    }


serviceKey : Mbta.Service -> ServiceKey
serviceKey service =
    { description = service.description
    , serviceType = service.serviceType
    , name = service.name
    , typicality = service.typicality
    , startDate = service.startDate
    , endDate = service.endDate
    , validDays = service.validDays
    , addedDates = service.addedDates
    , removedDates = service.removedDates
    }
