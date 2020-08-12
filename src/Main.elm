module Main exposing (main)

import Browser
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import List
import Process
import Random
import Task


type alias Robot =
    { id : RobotId
    , action : Maybe Action
    , lastProduced : Int
    , foobarsForSell : List FooBarId
    }


type RobotId
    = RobotId Int


type FooId
    = FooId RobotId Int


type BarId
    = BarId RobotId Int


type FooBarId
    = FooBarId RobotId Int FooId BarId


type Action
    = CreatingFoo
    | CreatingBar
    | CreatingFooBar FooId BarId
    | SellingFooBar (List FooBarId)


type alias Model =
    { robots : List Robot
    , balance : Int
    , foos : List FooId
    , bars : List BarId
    , foobars : List FooBarId
    }


type Msg
    = ActionClicked RobotId Action
    | BuyRobotClicked
      -- FOO PROCESS
    | FooCreated FooId
      -- BAR PROCESS
    | GotBarCreationTime BarId Float
    | BarCreated BarId
      -- FOOBAR PROCESS
    | FooBarDelayElapsed FooBarId
    | GotFooBarSuccess FooBarId Bool
      -- SELLING PROCESS
    | AddFooBar RobotId FooBarId
    | RemoveFooBar RobotId FooBarId
    | SoldFooBar RobotId Int


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


initModel : Model
initModel =
    { robots = List.map initRobot (List.reverse <| List.range 1 10) -- [ initRobot 2, initRobot 1 ]
    , balance = 0
    , foos = []
    , bars = []
    , foobars = []
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ActionClicked robotId action ->
            let
                ( newRobots, cmd ) =
                    model.robots
                        |> List.map
                            (\robot ->
                                if robot.id == robotId then
                                    setRobotAction action robot

                                else
                                    ( robot, Cmd.none )
                            )
                        |> List.unzip
                        |> Tuple.mapSecond Cmd.batch

                ( foos, bars, foobars ) =
                    removeFooBar action model
            in
            ( { model
                | robots = newRobots
                , foos = foos
                , bars = bars
                , foobars = foobars
              }
            , cmd
            )

        BuyRobotClicked ->
            ( { model
                | balance = model.balance - robotMoneyCost
                , foos = List.drop robotFooCost model.foos
                , robots = addRobot model.robots
              }
            , Cmd.none
            )

        -- FOO PROCESS
        FooCreated ((FooId robotId _) as fooId) ->
            ( { model
                | foos = fooId :: model.foos
                , robots = rest robotId model.robots
              }
            , Cmd.none
            )

        -- BAR PROCESS
        BarCreated ((BarId robotId _) as barId) ->
            ( { model
                | bars = barId :: model.bars
                , robots = rest robotId model.robots
              }
            , Cmd.none
            )

        GotBarCreationTime barId time ->
            ( model
            , Task.perform
                (always <| BarCreated barId)
                -- in 1 s
                (Process.sleep time)
            )

        -- FOO BAR PROCESS
        GotFooBarSuccess ((FooBarId robotId _ _ _) as fooBarId) True ->
            ( { model
                | robots = rest robotId model.robots
                , foobars = fooBarId :: model.foobars
              }
            , Cmd.none
            )

        FooBarDelayElapsed foobarId ->
            ( model
            , Random.generate (GotFooBarSuccess foobarId)
                (Random.weighted ( 0.6, True ) [ ( 0.4, False ) ])
            )

        GotFooBarSuccess (FooBarId robotId _ _ barId) False ->
            ( { model
                | robots = rest robotId model.robots
                , bars = barId :: model.bars
              }
            , Cmd.none
            )

        AddFooBar robotId foobar ->
            ( { model
                | foobars = List.filter ((/=) foobar) model.foobars
                , robots = addFoobarForSell robotId foobar model.robots
              }
            , Cmd.none
            )

        RemoveFooBar robotId foobar ->
            ( { model
                | foobars = foobar :: model.foobars
                , robots = removeFoobarForSell robotId foobar model.robots
              }
            , Cmd.none
            )

        SoldFooBar robotId fooBarCount ->
            ( { model
                | robots = rest robotId model.robots
                , balance = model.balance + fooBarCount * fooBarPrice
              }
            , Cmd.none
            )


setRobotAction : Action -> Robot -> ( Robot, Cmd Msg )
setRobotAction action robot =
    let
        newProduced =
            robot.lastProduced + 1

        newRobot =
            { robot | lastProduced = newProduced, action = Just action }
    in
    case action of
        CreatingFoo ->
            ( newRobot
            , Task.perform
                (always <| FooCreated <| FooId robot.id newProduced)
                -- in 1 s
                (Process.sleep 1000)
            )

        CreatingBar ->
            ( newRobot
            , Random.generate (GotBarCreationTime (BarId robot.id newProduced))
                (Random.float 500 2000)
            )

        CreatingFooBar fooId barId ->
            ( newRobot
            , Task.perform
                (always <| FooBarDelayElapsed <| FooBarId robot.id newProduced fooId barId)
                -- in 2 s
                (Process.sleep 2000)
            )

        SellingFooBar fooBarIds ->
            ( -- For this case, we don't need to increment the last produced field
              { robot | action = Just <| action, foobarsForSell = [] }
            , Task.perform
                (always <| SoldFooBar robot.id (List.length fooBarIds))
                -- in 10 s
                (Process.sleep 10000)
            )


removeFooBar : Action -> Model -> ( List FooId, List BarId, List FooBarId )
removeFooBar action model =
    case action of
        CreatingFoo ->
            ( model.foos, model.bars, model.foobars )

        CreatingBar ->
            ( model.foos, model.bars, model.foobars )

        CreatingFooBar fooId barId ->
            ( List.filter ((/=) fooId) model.foos
            , List.filter ((/=) barId) model.bars
            , model.foobars
            )

        SellingFooBar fooBarIds ->
            ( model.foos
            , model.bars
            , List.filter (\foobar -> not <| List.member foobar fooBarIds)
                model.foobars
            )


addFoobarForSell : RobotId -> FooBarId -> List Robot -> List Robot
addFoobarForSell robotId foobarId robots =
    robots
        |> List.map
            (\robot ->
                if robot.id == robotId then
                    { robot | foobarsForSell = foobarId :: robot.foobarsForSell }

                else
                    robot
            )


removeFoobarForSell : RobotId -> FooBarId -> List Robot -> List Robot
removeFoobarForSell robotId foobarId robots =
    robots
        |> List.map
            (\robot ->
                if robot.id == robotId then
                    { robot | foobarsForSell = List.filter ((/=) foobarId) robot.foobarsForSell }

                else
                    robot
            )


rest : RobotId -> List Robot -> List Robot
rest robotId robots =
    robots
        |> List.map
            (\robot ->
                if robot.id == robotId then
                    { robot | action = Nothing }

                else
                    robot
            )


initRobot : Int -> Robot
initRobot id =
    { id = RobotId id
    , action = Nothing
    , lastProduced = 0
    , foobarsForSell = []
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
    if List.length model.robots >= numberRobotToWin then
        H.div [ HA.class "font-bold text-2xl" ]
            [ H.text "CONGRATS!" ]

    else
        H.div [ HA.class "flex flex-col pt-4 h-screen min-h-0" ]
            [ viewStock model
            , viewBuyRobot model
            , H.div [ HA.class "flex flex-col mt-8 pt-4 overflow-auto border-t border-gray-400 flex-grow" ]
                [ H.div [ HA.class "flex flex-row flex-wrap pb-8" ]
                    (List.map (viewRobot model) (List.reverse model.robots))
                ]
            ]


viewStock : Model -> Html Msg
viewStock model =
    H.div [ HA.class "flex flex-row" ]
        [ viewResourceCard
            { iconName = "euro_symbol"
            , hardColor = "yellow-700"
            , softColor = "yellow-200"
            , title = "MONEY"
            , value = "€" ++ String.fromInt model.balance
            }
        , viewResourceCard
            { iconName = "widgets"
            , hardColor = "teal-700"
            , softColor = "teal-200"
            , title = "FOO"
            , value = String.fromInt (List.length model.foos)
            }
        , viewResourceCard
            { iconName = "device_hub"
            , hardColor = "red-700"
            , softColor = "red-200"
            , title = "BAR"
            , value = String.fromInt (List.length model.bars)
            }
        , viewResourceCard
            { iconName = "science"
            , hardColor = "green-700"
            , softColor = "green-200"
            , title = "FOO-BAR"
            , value = String.fromInt (List.length model.foobars)
            }
        ]


viewBuyRobot : Model -> Html Msg
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
                if model.balance >= robotMoneyCost && List.length model.foos >= robotFooCost then
                    [ activeButton "BUY ROBOT" BuyRobotClicked
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


viewRobot : Model -> Robot -> Html Msg
viewRobot model robot =
    H.div [ HA.class "w-1/4 px-4 pt-8" ]
        [ H.div [ HA.class "flex flex-col space-y-4 p-4 border rounded-md h-48 shadow-md" ]
            [ H.h2 [ HA.class "font-bold" ]
                [ H.text <| "Foobarer n°" ++ showRobotId robot.id ]
            , case robot.action of
                Nothing ->
                    viewActions model robot

                Just CreatingFoo ->
                    H.text "Mining Foo ..."

                Just CreatingBar ->
                    H.text <| "Mining bar ..."

                Just (CreatingFooBar fooId barId) ->
                    H.text <| "Creating foobar from " ++ showFooId fooId ++ " and " ++ showBarId barId ++ "..."

                Just (SellingFooBar fooBars) ->
                    H.text <| "Selling foobars: " ++ String.join "," (List.map showFooBarId fooBars) ++ "..."
            ]
        ]


viewActions : Model -> Robot -> Html Msg
viewActions model robot =
    H.div [ HA.class "flex flex-col space-y-4 text-sm" ]
        [ H.div [ HA.class "flex flex-row justify-around" ]
            [ activeButton
                "MINE FOO"
                (ActionClicked robot.id CreatingFoo)
            , activeButton
                "MINE BAR"
                (ActionClicked robot.id CreatingBar)
            , case ( model.foos, model.bars ) of
                ( fooId :: _, barId :: _ ) ->
                    activeButton
                        "BUILD FOO-BAR"
                        (ActionClicked robot.id <| CreatingFooBar fooId barId)

                _ ->
                    inactiveButton
                        "BUILD FOO-BAR"
            ]
        , H.div [ HA.class "flex flex-row justify-center space-x-4" ]
            [ case robot.foobarsForSell of
                foobar :: _ ->
                    activeButton
                        "-"
                        (RemoveFooBar robot.id foobar)

                _ ->
                    inactiveButton
                        "-"
            , if not <| List.isEmpty robot.foobarsForSell then
                activeButton
                    ("SELL " ++ String.fromInt (List.length robot.foobarsForSell) ++ " FOO-BARS")
                    (ActionClicked robot.id <| SellingFooBar robot.foobarsForSell)

              else
                inactiveButton
                    "SELL 0 FOO-BARS"
            , case ( model.foobars, List.length robot.foobarsForSell < maxSellingFoobarCount ) of
                ( foobar :: _, True ) ->
                    activeButton
                        "+"
                        (AddFooBar robot.id foobar)

                _ ->
                    inactiveButton
                        "+"
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


main : Program () Model Msg
main =
    Browser.element
        { init = always ( initModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
