module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Date exposing (Date)
import Lamdera exposing (ClientId)
import Url exposing (Url)


type Route
    = Home (Maybe Date)
    | Admin
    | Archive
    | NotFound


type Frequency
    = OneTime
    | Weekly
    | BiWeekly
    | Monthly


type PaymentType
    = Automatic
    | Manual


type Amount
    = Debit Float
    | Credit Float


type Status
    = Active
    | Inactive Date


type alias Definition =
    { description : String
    , amount : Amount
    , paymentType : PaymentType
    , startDate : Date
    , frequency : Frequency
    }


type ItemAction
    = Paid
    | Skipped


type alias Item =
    { description : String
    , amount : Amount
    , paymentType : PaymentType
    , date : Date
    }


type alias ActionedItem =
    { description : String
    , amount : Amount
    , paymentType : PaymentType
    , date : Date
    , action : ItemAction
    , actionedDate : Date
    }


type alias ReadyModel =
    { key : Key
    , items : List Item
    , definitions : List Definition
    , archive : List ActionedItem
    , endDate : Date
    , route : Route
    }


type FrontendModel
    = Initializing { key : Key, initialRoute : Route }
    | Ready ReadyModel


key : FrontendModel -> Key
key model =
    case model of
        Initializing m ->
            m.key

        Ready m ->
            m.key


type alias BackendModel =
    { definitions : List Definition
    , archive : List ActionedItem
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | GotToday Date
    | ChangeEndDate String
    | Pay Item
    | Skip Item
    | Undo ActionedItem


type ToBackend
    = GetItems Date
    | GetArchive
    | Unarchive ActionedItem
    | PayItem Item
    | SkipItem Item


type BackendMsg
    = GotDateForAction ClientId ItemAction Item Date


type ToFrontend
    = Items (List Item)
    | ArchiveItems (List ActionedItem)
    | ItemPaid Item
    | ItemSkipped Item
