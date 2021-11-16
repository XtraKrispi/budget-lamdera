module TestData exposing (..)

import Date exposing (fromCalendarDate, toIsoString)
import Time exposing (Month(..))
import Types exposing (..)


testDefinition : Definition
testDefinition =
    { description = "Test"
    , amount = Debit 50
    , paymentType = Automatic
    , startDate = fromCalendarDate 2021 Nov 1
    , frequency = BiWeekly
    }


testCreditDefinition : Definition
testCreditDefinition =
    { description = "Test Credit"
    , amount = Credit 4500
    , paymentType = Automatic
    , startDate = fromCalendarDate 2021 Nov 5
    , frequency = BiWeekly
    }


itemDates : List Item -> List String
itemDates =
    List.map (toIsoString << .date)
