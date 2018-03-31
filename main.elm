import Html exposing (..)
import Html.Events exposing (onInput)
import Html.Attributes exposing (class, attribute)

import Date
import Time

main : Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

--TODO: I18n
type alias MonetaryValue = {
  pounds : Int,
  pence : Int
}

type InputData a       = NoInputValue  | InputValue a
type CalculatedData a  = MissingInputs String | CalculatedValue a

type alias TotalOwed      = InputData MonetaryValue
type alias StartDate      = InputData Date.Date
type alias EndDate        = InputData Date.Date
type alias CurrentTime    = Maybe Time.Time
type alias RemainderOwed  = CalculatedData MonetaryValue

type alias Model =
  { totalOwed   : TotalOwed
  , startDate   : StartDate
  , endDate     : EndDate
  , currentTime : CurrentTime
  , remainder   : RemainderOwed
}

init : (Model, Cmd Msg)
init = (
  { totalOwed   = NoInputValue --InputValue <| {pounds = 410000, pence = 0}
  , startDate   = NoInputValue
  , endDate     = NoInputValue
  , currentTime = Nothing
  , remainder   = MissingInputs ""
  }, Cmd.none)

-- UPDATE

divideMoneyByTime : MonetaryValue -> Float -> MonetaryValue
divideMoneyByTime mV t =
  let
    mVInPence = (100 * mV.pounds) + mV.pence
    division = (toFloat mVInPence) / t
    pounds = floor <| division / 100
    penies = (ceiling division) - (pounds * 100)
  in
    {pounds = pounds, pence = penies}

multiplyMoneyByTime : MonetaryValue -> Float -> MonetaryValue
multiplyMoneyByTime mV t =
  let
    mVInPence = (100 * mV.pounds) + mV.pence
    multiplication = (toFloat mVInPence) * t
    pounds = floor <| multiplication / 100
    penies = (ceiling multiplication) - (pounds * 100)
  in
    {pounds = pounds, pence = penies}

moneySubtractMoney : MonetaryValue -> MonetaryValue -> MonetaryValue
moneySubtractMoney a b =
  let
    aInPenies = (a.pounds * 100) + a.pence
    bInPenies = (b.pounds * 100) + b.pence
    subtraction = aInPenies - bInPenies
    pounds = floor <| (toFloat subtraction) / 100
    penies = (ceiling (toFloat subtraction)) - (pounds * 100)
  in
    {pounds = pounds, pence = penies}

calculateRemainder : TotalOwed -> StartDate -> EndDate -> CurrentTime -> RemainderOwed
calculateRemainder totalOwed startDate endDate currentTime =
  case startDate of
    NoInputValue -> MissingInputs "Start Date"
    InputValue startDate_ -> case endDate of
        NoInputValue -> MissingInputs "End Date"
        InputValue endDate_ -> case totalOwed of
          NoInputValue -> MissingInputs "Total Original Debt"
          InputValue totalOwed_ -> case currentTime of
               Nothing -> MissingInputs "Current Time"
               Just currentTime_ -> CalculatedValue <| actuallyCalculateRemainder totalOwed_ startDate_ endDate_ currentTime_

actuallyCalculateRemainder : MonetaryValue -> Date.Date -> Date.Date -> Time.Time -> MonetaryValue
actuallyCalculateRemainder totalOwed startDate endDate currentTime =
  let
    startTime = Date.toTime startDate
    endTime = Date.toTime endDate
    totalTime = (Time.inSeconds endTime) - (Time.inSeconds startTime)
    moneyPerUnitTime = divideMoneyByTime totalOwed totalTime
    elapsedTime = (Time.inSeconds currentTime) - (Time.inSeconds startTime)
    remainder = moneySubtractMoney totalOwed (multiplyMoneyByTime totalOwed (elapsedTime / totalTime))
  in
    remainder

type Msg
  = Tick Time.Time
  | UpdateOutput
  | NewStartDate String
  | NewEndDate String
  | NewTotalDebt String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick time ->
      ({ model | currentTime = Just time, remainder = calculateRemainder model.totalOwed model.startDate model.endDate model.currentTime }, Cmd.none)
    UpdateOutput ->
      ({ model | remainder = calculateRemainder model.totalOwed model.startDate model.endDate model.currentTime }, Cmd.none)
    NewStartDate str -> case Date.fromString str of
      Err err -> ({ model | startDate = NoInputValue }, Cmd.none)
      Ok date -> ({ model | startDate = InputValue date }, Cmd.none)
    NewEndDate str -> case Date.fromString str of
      Err err -> ({ model | endDate = NoInputValue }, Cmd.none)
      Ok date -> ({ model | endDate = InputValue date }, Cmd.none)
    NewTotalDebt str -> case String.toInt str of
      Err err -> ({ model | totalOwed = NoInputValue }, Cmd.none)
      Ok amount -> ({ model | totalOwed = InputValue {pounds = amount, pence = 0} }, Cmd.none)


-- VIEW

remainderAsHtml : RemainderOwed -> Html Msg
remainderAsHtml remainder =
  let
    leftPad : String -> String
    leftPad str = if String.length str == 1 then "0" ++ str else str

    commas : String -> String
    commas str = if String.length str > 3 then (String.slice 0 -3 str) ++ "," ++ (String.slice -3 (String.length str) str) else str
  in
    case remainder of
    MissingInputs str -> Html.text <| "Missing inputs: " ++ str
    CalculatedValue m -> Html.text <| (commas (toString m.pounds)) ++ "." ++ (leftPad (toString m.pence))

view : Model -> Html Msg
view model =
  main_ []
    [ div [ class "outputs" ]
        [ div [ class "output" ]
          [ h1 [] [ remainderAsHtml (model.remainder) ]
          ]
        ]
    , div [ class "inputs" ]
        [ label []
          [ text "Start Date"
          , input [onInput NewStartDate, attribute "placeholder" "YYYY-MM-DD"] []
          ]
        , label []
          [ text "End Date"
          , input [onInput NewEndDate, attribute "placeholder" "YYYY-MM-DD"] []
          ]
        , label []
          [ text "Total Original Debt"
          , input [onInput NewTotalDebt, attribute "placeholder" "#####"] []
          ]
        ]

    ]

-- PORTS

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every Time.second Tick
