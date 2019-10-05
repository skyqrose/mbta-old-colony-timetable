module Model exposing (Model, Msg(..), routeIds, stopIds)

import Mbta
import Mbta.Api
import RemoteData


type alias Model =
    { routes : RemoteDataApi (List Mbta.Route)
    , stops : RemoteDataApi (List Mbta.Stop)
    , schedules : RemoteDataApi (List Mbta.Schedule)
    , services : RemoteDataApi (List Mbta.Service)
    , selectedServiceKey : Maybe ServiceKey
    }


type Msg
    = ReceiveRoutes (Mbta.Api.ApiResult (List Mbta.Route))
    | ReceiveStops (Mbta.Api.ApiResult (List Mbta.Stop))
    | ReceiveSchedules (Mbta.Api.ApiResult (List Mbta.Schedule))
    | ReceiveServices (Mbta.Api.ApiResult (List Mbta.Service))


type alias RemoteDataApi primary =
    RemoteData.RemoteData Mbta.Api.ApiError (Mbta.Api.Data primary)


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
