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
    , lockAction : Bool
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
    | LockAction RobotId


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


initModel : Model
initModel =
    { robots = [ initRobot 2, initRobot 1 ]
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
            let
                newModel =
                    { model
                        | foos = fooId :: model.foos
                        , robots = rest robotId model.robots
                    }
            in
            if getRobotLock robotId model.robots && List.length model.foos < 20 then
                update (ActionClicked robotId CreatingFoo) newModel

            else
                ( newModel
                , Cmd.none
                )

        -- BAR PROCESS
        BarCreated ((BarId robotId _) as barId) ->
            let
                newModel =
                    { model
                        | bars = barId :: model.bars
                        , robots = rest robotId model.robots
                    }
            in
            if getRobotLock robotId model.robots && List.length model.bars < 20 then
                update (ActionClicked robotId CreatingBar) newModel

            else
                ( newModel
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
        FooBarDelayElapsed foobarId ->
            ( model
            , Random.generate (GotFooBarSuccess foobarId)
                (Random.weighted ( 0.6, True ) [ ( 0.4, False ) ])
            )

        GotFooBarSuccess ((FooBarId robotId _ _ _) as fooBarId) True ->
            let
                newModel =
                    { model
                        | robots = rest robotId model.robots
                        , foobars = fooBarId :: model.foobars
                    }
            in
            if getRobotLock robotId model.robots then
                case ( model.foos, model.bars ) of
                    ( newFooId :: _, newBarId :: _ ) ->
                        update (ActionClicked robotId (CreatingFooBar newFooId newBarId)) newModel

                    _ ->
                        ( newModel, Cmd.none )

            else
                ( newModel, Cmd.none )

        GotFooBarSuccess (FooBarId robotId _ _ barId) False ->
            let
                newModel =
                    { model
                        | robots = rest robotId model.robots
                        , bars = barId :: model.bars
                    }
            in
            if getRobotLock robotId model.robots then
                case ( model.foos, model.bars ) of
                    ( newFooId :: _, newBarId :: _ ) ->
                        update (ActionClicked robotId (CreatingFooBar newFooId newBarId)) newModel

                    _ ->
                        ( newModel, Cmd.none )

            else
                ( newModel, Cmd.none )

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

        LockAction robotId ->
            ( { model
                | robots = setRobotLock robotId model.robots
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


getRobotLock : RobotId -> List Robot -> Bool
getRobotLock robotId robots =
    robots
        |> List.filter (\robot -> robot.id == robotId)
        |> List.map .lockAction
        |> List.head
        |> Maybe.withDefault False


setRobotLock : RobotId -> List Robot -> List Robot
setRobotLock robotId robots =
    robots
        |> List.map
            (\robot ->
                if robot.id == robotId then
                    { robot | lockAction = not robot.lockAction }

                else
                    robot
            )


initRobot : Int -> Robot
initRobot id =
    { id = RobotId id
    , action = Nothing
    , lastProduced = 0
    , foobarsForSell = []
    , lockAction = False
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
    if List.length model.robots >= 30 then
        H.div [ HA.class "font-bold text-2xl" ]
            [ H.text "CONGRATS!" ]

    else
        H.div [ HA.class "flex flex-row space-x-8 mt-4" ]
            [ H.div [ HA.class "flex flex-col space-y-4 ml-4" ]
                (List.map (viewRobot model) model.robots)
            , viewStock model
            ]


viewStock : Model -> Html Msg
viewStock model =
    H.div [ HA.class "flex flex-col" ]
        ([ H.text <| String.fromInt model.balance ++ "€"
         , H.text <| "FOO COUNT: " ++ String.fromInt (List.length model.foos)
         , H.text <| "BAR COUNT: " ++ String.fromInt (List.length model.bars)
         , H.text <| "FOO-BAR COUNT: " ++ String.fromInt (List.length model.foobars)
         , if model.balance >= robotMoneyCost && List.length model.foos > robotFooCost then
            H.button
                [ HA.class "rounded-md bg-green-500"
                , HE.onClick BuyRobotClicked
                ]
                [ H.text "BUY ROBOT" ]

           else
            H.button
                [ HA.class "rounded-md bg-gray-400 cursor-not-allowed"
                , HA.disabled True
                ]
                [ H.text "BUY ROBOT" ]
         ]
            |> List.map (H.div [] << List.singleton)
        )


viewRobot : Model -> Robot -> Html Msg
viewRobot model robot =
    H.div [ HA.class "flex flex-col space-y-4 p-4 border rounded-md h-40" ]
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


viewActions : Model -> Robot -> Html Msg
viewActions model robot =
    H.div [ HA.class "flex flex-col space-y-4 text-sm" ]
        [ H.div [ HA.class "flex flex-row space-x-4" ]
            [ H.button
                [ HA.class "rounded-md bg-green-500"
                , HE.onClick (ActionClicked robot.id CreatingFoo)
                ]
                [ H.text "MINE FOO" ]
            , H.button
                [ HA.class "rounded-md bg-green-500"
                , HE.onClick (ActionClicked robot.id CreatingBar)
                ]
                [ H.text "MINE BAR" ]
            , case ( model.foos, model.bars ) of
                ( fooId :: _, barId :: _ ) ->
                    H.button
                        [ HA.class "rounded-md bg-green-500"
                        , HE.onClick (ActionClicked robot.id <| CreatingFooBar fooId barId)
                        ]
                        [ H.text "BUILD FOO-BAR" ]

                _ ->
                    H.button
                        [ HA.class "rounded-md bg-gray-400 cursor-not-allowed"
                        , HA.disabled True
                        ]
                        [ H.text "BUILD FOO-BAR" ]
            ]
        , H.div [ HA.class "flex flex-row space-x-4" ]
            [ case robot.foobarsForSell of
                foobar :: _ ->
                    H.button
                        [ HA.class "rounded-md bg-green-500 px-1"
                        , HE.onClick (RemoveFooBar robot.id foobar)
                        ]
                        [ H.text "-" ]

                _ ->
                    H.button
                        [ HA.class "rounded-md bg-gray-400 px-1 cursor-not-allowed"
                        , HA.disabled True
                        ]
                        [ H.text "-" ]
            , if not <| List.isEmpty robot.foobarsForSell then
                H.button
                    [ HA.class "rounded-md bg-green-500"
                    , HE.onClick (ActionClicked robot.id <| SellingFooBar robot.foobarsForSell)
                    ]
                    [ H.text <| "SELL " ++ String.fromInt (List.length robot.foobarsForSell) ++ " FOO-BARS" ]

              else
                H.button
                    [ HA.class "rounded-md bg-gray-400 cursor-not-allowed"
                    , HA.disabled True
                    ]
                    [ H.text "SELL 0 FOO-BARS" ]
            , case ( model.foobars, List.length robot.foobarsForSell < maxSellingFoobarCount ) of
                ( foobar :: _, True ) ->
                    H.button
                        [ HA.class "rounded-md bg-green-500 px-1"
                        , HE.onClick (AddFooBar robot.id foobar)
                        ]
                        [ H.text "+" ]

                _ ->
                    H.button
                        [ HA.class "rounded-md bg-gray-400 px-1 cursor-not-allowed"
                        , HA.disabled True
                        ]
                        [ H.text "+" ]
            ]
        , H.label []
            [ H.input
                [ HA.type_ "checkbox"
                , HA.checked robot.lockAction
                , HE.onClick (LockAction robot.id)
                ]
                []
            , H.text " Lock action"
            ]
        ]


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
