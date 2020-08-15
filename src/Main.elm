module Main exposing (main)

import Browser
import Browser.Events
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import List
import Process
import Random
import Task
import Tuple


type alias Robot =
    { id : RobotId
    , order : Order
    , action : Maybe Action
    , lastProduced : Int
    }


type Order
    = CreateFoo
    | CreateBar
    | CreateFooBar
    | SellFooBar


type RobotId
    = RobotId Int


type FooId
    = FooId RobotId Int


type BarId
    = BarId RobotId Int


type FooBarId
    = FooBarId RobotId Int FooId BarId


type alias Action =
    { elapsedMs : Float
    , totalMs : Float
    , kind : ActionKind
    }


type ActionKind
    = CreatingFoo
    | CreatingBar
    | CreatingFooBar FooId BarId
    | FailingAtCreatingFooBar FooId BarId
    | SellingFooBar (List FooBarId)


type Model
    = Loading
    | Loaded LoadedModel


type alias LoadedModel =
    { robots : List Robot
    , stock : Stock
    , seed : Random.Seed
    }


type Msg
    = GotSeed Random.Seed
    | OrderClicked RobotId Order
    | BuyRobotClicked Stock
    | GotNewFrame Float


robotMoneyCost : Int
robotMoneyCost =
    3


robotFooCost : Int
robotFooCost =
    6


fooBarPrice : Int
fooBarPrice =
    1


maxSellingFoobarCount : Int
maxSellingFoobarCount =
    5


numberRobotToWin : Int
numberRobotToWin =
    30


init : ( Model, Cmd Msg )
init =
    ( Loading
    , Random.generate GotSeed Random.independentSeed
    )


initFromSeed : Random.Seed -> LoadedModel
initFromSeed seed =
    { robots = [ initRobot 2, initRobot 1 ]
    , stock = emptyStock
    , seed = seed
    }


updateTime : Float -> Robot -> ( Robot, Stock )
updateTime delta robot =
    case robot.action of
        Nothing ->
            ( robot, emptyStock )

        Just action ->
            let
                newElapsed =
                    action.elapsedMs + delta
            in
            if newElapsed > action.totalMs then
                actionCompleted robot action.kind

            else
                ( { robot | action = Just { action | elapsedMs = newElapsed } }
                , emptyStock
                )


type alias Stock =
    { foos : List FooId
    , bars : List BarId
    , foobars : List FooBarId
    , balance : Int
    }


merge : Stock -> Stock -> Stock
merge a1 a2 =
    { foos = a1.foos ++ a2.foos
    , bars = a1.bars ++ a2.bars
    , foobars = a1.foobars ++ a2.foobars
    , balance = a1.balance + a2.balance
    }


emptyStock : Stock
emptyStock =
    { foos = []
    , bars = []
    , foobars = []
    , balance = 0
    }


actionCompleted : Robot -> ActionKind -> ( Robot, Stock )
actionCompleted robot kind =
    let
        newProduced =
            robot.lastProduced + 1

        newRobot =
            { robot | lastProduced = newProduced, action = Nothing }
    in
    case kind of
        CreatingFoo ->
            ( newRobot, { emptyStock | foos = [ FooId robot.id newProduced ] } )

        CreatingBar ->
            ( newRobot, { emptyStock | bars = [ BarId robot.id newProduced ] } )

        CreatingFooBar fooId barId ->
            ( newRobot, { emptyStock | foobars = [ FooBarId robot.id newProduced fooId barId ] } )

        FailingAtCreatingFooBar fooId barId ->
            ( newRobot, { emptyStock | bars = [ barId ] } )

        SellingFooBar foobars ->
            ( newRobot, { emptyStock | balance = fooBarPrice * List.length foobars } )


startRobotAction : Robot -> ( List Robot, Stock, Random.Seed ) -> ( List Robot, Stock, Random.Seed )
startRobotAction robot ( robots, stock, seed ) =
    case robot.action of
        Just _ ->
            -- The robot already is performing an action, so don't change anything!
            ( robot :: robots, stock, seed )

        Nothing ->
            case robot.order of
                CreateFoo ->
                    ( { robot | action = Just { elapsedMs = 0, totalMs = 1000, kind = CreatingFoo } } :: robots
                    , stock
                    , seed
                    )

                CreateBar ->
                    let
                        ( time, newSeed ) =
                            Random.step (Random.float 500 2500) seed
                    in
                    ( { robot | action = Just { elapsedMs = 0, totalMs = time, kind = CreatingBar } } :: robots
                    , stock
                    , newSeed
                    )

                CreateFooBar ->
                    case ( stock.foos, stock.bars ) of
                        ( foo :: foos, bar :: bars ) ->
                            let
                                ( success, newSeed ) =
                                    Random.step (Random.weighted ( 0.6, True ) [ ( 0.4, False ) ]) seed

                                action =
                                    if success then
                                        CreatingFooBar foo bar

                                    else
                                        FailingAtCreatingFooBar foo bar
                            in
                            ( { robot | action = Just { elapsedMs = 0, totalMs = 2000, kind = action } } :: robots
                            , { stock | foos = foos, bars = bars }
                            , newSeed
                            )

                        _ ->
                            ( robot :: robots, stock, seed )

                SellFooBar ->
                    if not <| List.isEmpty stock.foobars then
                        ( { robot | action = Just { elapsedMs = 0, totalMs = 10 * 1000, kind = SellingFooBar <| List.take 5 stock.foobars } } :: robots
                        , { stock | foobars = List.drop 5 stock.foobars }
                        , seed
                        )

                    else
                        ( robot :: robots, stock, seed )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( GotSeed seed, Loading ) ->
            ( Loaded <| initFromSeed seed
            , Cmd.none
            )

        ( _, Loaded loadedModel ) ->
            updateLoaded msg loadedModel
                |> Tuple.mapFirst Loaded

        _ ->
            ( model, Cmd.none )


updateLoaded : Msg -> LoadedModel -> ( LoadedModel, Cmd Msg )
updateLoaded msg model =
    case msg of
        OrderClicked robotId order ->
            ( { model | robots = updateRobot robotId (\r -> { r | order = order }) model.robots }
            , Cmd.none
            )

        BuyRobotClicked newStock ->
            ( { model
                | stock = newStock
                , robots = addRobot model.robots
              }
            , Cmd.none
            )

        GotNewFrame delta ->
            let
                ( tempRobots, tempStock ) =
                    List.map (updateTime delta) model.robots
                        |> List.unzip
                        |> Tuple.mapSecond (List.foldr merge model.stock)

                ( newRobots, newStock, newSeed ) =
                    List.foldr startRobotAction ( [], tempStock, model.seed ) tempRobots
            in
            ( { model | robots = newRobots, stock = newStock, seed = newSeed }
            , Cmd.none
            )

        GotSeed seed ->
            -- should not happen!
            ( model
            , Cmd.none
            )


updateRobot : RobotId -> (Robot -> Robot) -> List Robot -> List Robot
updateRobot robotId f =
    List.map
        (\robot ->
            if robot.id == robotId then
                f robot

            else
                robot
        )


initRobot : Int -> Robot
initRobot id =
    { id = RobotId id
    , order = CreateFoo
    , action = Nothing
    , lastProduced = 0
    }


addRobot : List Robot -> List Robot
addRobot robots =
    let
        n =
            List.length robots
    in
    initRobot (n + 1) :: robots



--------
-- VIEW
--------


view : Model -> Html Msg
view model =
    case model of
        Loading ->
            -- This state should not last more than a few ms, don't show anything!
            H.text ""

        Loaded loadedModel ->
            if List.length loadedModel.robots >= numberRobotToWin then
                H.div [ HA.class "font-bold text-2xl" ]
                    [ H.text "CONGRATS!" ]

            else
                H.div [ HA.class "flex flex-col pt-4 h-screen min-h-0" ]
                    [ viewStock loadedModel.stock
                    , viewBuyRobot loadedModel
                    , H.div [ HA.class "flex flex-col mt-8 pt-4 overflow-auto border-t border-gray-400 flex-grow" ]
                        [ H.div [ HA.class "flex flex-row flex-wrap pb-8" ]
                            (List.map viewRobot (List.reverse loadedModel.robots))
                        ]
                    ]


viewStock : Stock -> Html Msg
viewStock stock =
    H.div [ HA.class "flex flex-row" ]
        [ viewResourceCard
            { iconName = "euro_symbol"
            , hardColor = "yellow-700"
            , softColor = "yellow-200"
            , title = "MONEY"
            , value = "€" ++ String.fromInt stock.balance
            }
        , viewResourceCard
            { iconName = "widgets"
            , hardColor = "teal-700"
            , softColor = "teal-200"
            , title = "FOO"
            , value = String.fromInt (List.length stock.foos)
            }
        , viewResourceCard
            { iconName = "device_hub"
            , hardColor = "red-700"
            , softColor = "red-200"
            , title = "BAR"
            , value = String.fromInt (List.length stock.bars)
            }
        , viewResourceCard
            { iconName = "science"
            , hardColor = "green-700"
            , softColor = "green-200"
            , title = "FOO-BAR"
            , value = String.fromInt (List.length stock.foobars)
            }
        ]


viewBuyRobot : LoadedModel -> Html Msg
viewBuyRobot model =
    let
        remainingRobots =
            numberRobotToWin - List.length model.robots

        robotStr =
            if remainingRobots > 1 then
                "ROBOTS"

            else
                "ROBOT"
    in
    H.div [ HA.class "flex flex-col items-center mt-16" ]
        [ H.div [ HA.class "flex flex-row justify-center space-x-16 items-center" ]
            [ H.div [ HA.class "flex flex-col items-end leading-none" ]
                [ H.span [ HA.class "font-bold text-gray-500 text-sm" ] [ H.text "YOU ARE" ]
                , H.span [ HA.class "font-bold text-green-700 text-4xl" ] [ H.text (String.fromInt remainingRobots) ]
                , H.span [ HA.class "font-bold text-gray-500 text-sm" ] [ H.text <| robotStr ++ " AWAY FROM VICTORY" ]
                ]
            , H.div [ HA.class "flex flex-col" ] <|
                if model.stock.balance >= robotMoneyCost && List.length model.stock.foos >= robotFooCost then
                    let
                        stock =
                            model.stock

                        newStock =
                            { stock
                                | balance = stock.balance - robotMoneyCost
                                , foos = List.drop robotFooCost stock.foos
                            }
                    in
                    [ activeButton "BUY ROBOT" (BuyRobotClicked newStock)
                    , H.span [ HA.class "text-green-500 text-sm invisible" ]
                        [ H.text <| "You need €" ++ String.fromInt robotMoneyCost ++ " and " ++ String.fromInt robotFooCost ++ " foos."
                        ]
                    ]

                else
                    [ inactiveButton "BUY ROBOT"
                    , H.span [ HA.class "text-green-500 text-sm" ]
                        [ H.text <| "You need €" ++ String.fromInt robotMoneyCost ++ " and " ++ String.fromInt robotFooCost ++ " foos."
                        ]
                    ]
            ]
        ]


activeButton : String -> msg -> Html msg
activeButton text msg =
    H.button
        [ HA.class "rounded-md bg-green-600 px-5 py-2 text-green-200"
        , HE.onClick msg
        ]
        [ H.text text ]


inactiveButton : String -> Html msg
inactiveButton text =
    H.button
        [ HA.class "rounded-md bg-green-100 px-5 py-2 cursor-not-allowed text-green-500"
        , HA.disabled True
        ]
        [ H.text text ]


viewResourceCard :
    { iconName : String
    , hardColor : String
    , softColor : String
    , title : String
    , value : String
    }
    -> Html msg
viewResourceCard r =
    H.div [ HA.class "h-40 w-1/4 p-4" ]
        [ H.div
            [ HA.class "flex flex-row rounded-md p-10 space-x-16 shadow-xl"

            -- this way of dealing with Tailwind properties are not very good
            -- since we cannot safely "purge" the CSS file
            , HA.class <| "bg-" ++ r.softColor
            , HA.class <| "border-b-4 border-" ++ r.hardColor
            ]
            [ viewIcon [ HA.class <| "rounded-full text-white p-6 bg-" ++ r.hardColor ] r.iconName
            , H.div [ HA.class "flex flex-col mt-4" ]
                [ H.div [ HA.class "text-gray-600 font-bold text-sm" ] [ H.text r.title ]
                , H.div [ HA.class "font-bold text-2xl" ] [ H.text r.value ]
                ]
            ]
        ]


viewRobot : Robot -> Html Msg
viewRobot robot =
    H.div [ HA.class "w-1/4 px-4 pt-8" ]
        [ H.div [ HA.class "flex flex-col space-y-4 p-4 border rounded-md h-48 shadow-md" ]
            [ H.h2 [ HA.class "font-bold" ]
                [ H.text <| "Foobarer n°" ++ showRobotId robot.id ]
            , viewOrders robot
            , case robot.action of
                Nothing ->
                    H.text "Waiting..."

                Just action ->
                    case action.kind of
                        CreatingFoo ->
                            H.text "Mining Foo..."

                        CreatingBar ->
                            H.text <| "Mining bar..."

                        CreatingFooBar fooId barId ->
                            H.text <| "Creating foobar from " ++ showFooId fooId ++ " and " ++ showBarId barId ++ "..."

                        FailingAtCreatingFooBar fooId barId ->
                            H.text <| "Creating foobar from " ++ showFooId fooId ++ " and " ++ showBarId barId ++ "..."

                        SellingFooBar fooBars ->
                            H.text <| "Selling foobars: " ++ String.join "," (List.map showFooBarId fooBars) ++ "..."
            ]
        ]


viewOrders : Robot -> Html Msg
viewOrders robot =
    H.div [ HA.class "flex flex-row flex wrap justify-around space-x-2 space-y-2" ]
        [ orderButton
            { selected = robot.order
            , displayed = CreateFoo
            }
            robot
            "MINE FOO"
        , orderButton
            { selected = robot.order
            , displayed = CreateBar
            }
            robot
            "MINE BAR"
        , orderButton
            { selected = robot.order
            , displayed = CreateFooBar
            }
            robot
            "BUILD FOOBAR"
        , orderButton
            { selected = robot.order
            , displayed = SellFooBar
            }
            robot
            "SELL FOOBAR"
        ]


orderButton : { selected : Order, displayed : Order } -> Robot -> String -> H.Html Msg
orderButton { selected, displayed } robot name =
    if selected == displayed then
        H.button
            [ HA.class "rounded-md bg-blue-600 px-5 py-2 text-blue-200 cursor-not-allowed"
            , HA.disabled True
            ]
            [ H.text name ]

    else
        H.button
            [ HA.class "rounded-md bg-blue-100 px-5 py-2 text-blue-500"
            , HE.onClick (OrderClicked robot.id displayed)
            ]
            [ H.text name ]


viewIcon : List (H.Attribute msg) -> String -> Html msg
viewIcon attrs name =
    H.i (HA.class "material-icons" :: attrs) [ H.text name ]


showRobotId : RobotId -> String
showRobotId (RobotId id) =
    String.fromInt id


showFooId : FooId -> String
showFooId (FooId robotId serialNumber) =
    "F-" ++ showRobotId robotId ++ "-" ++ String.fromInt serialNumber


showBarId : BarId -> String
showBarId (BarId robotId serialNumber) =
    "B-" ++ showRobotId robotId ++ "-" ++ String.fromInt serialNumber


showFooBarId : FooBarId -> String
showFooBarId (FooBarId robotId _ fooId barId) =
    "FB-" ++ showRobotId robotId ++ "-" ++ showFooId fooId ++ "-" ++ showBarId barId


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onAnimationFrameDelta GotNewFrame


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
