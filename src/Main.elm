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


type alias Dev =
    { id : DevId
    , order : Order
    , action : Maybe Action
    , lastProduced : Int
    }


type Order
    = CreateCoffee
    | CreateServer
    | CreateApp
    | SellApp


type DevId
    = DevId Int


type CoffeeId
    = CoffeeId DevId Int


type ServerId
    = ServerId DevId Int


type AppId
    = AppId DevId Int CoffeeId ServerId


type alias Action =
    { elapsedMs : Float
    , totalMs : Float
    , kind : ActionKind
    }


type ActionKind
    = CreatingCoffee
    | CreatingServer
    | CreatingApp CoffeeId ServerId
    | FailingAtCreatingApp CoffeeId ServerId
    | SellingApp (List AppId)


type Model
    = Loading
    | Loaded LoadedModel


type alias LoadedModel =
    { devs : List Dev
    , stock : Stock
    , seed : Random.Seed
    }


type Msg
    = GotSeed Random.Seed
    | OrderClicked DevId Order
    | RecruitDevClicked Stock
    | GotNewFrame Float


devMoneyCost : Int
devMoneyCost =
    3000


devCoffeeCost : Int
devCoffeeCost =
    6


appPrice : Int
appPrice =
    1000


maxSellingAppCount : Int
maxSellingAppCount =
    5


numberDevToWin : Int
numberDevToWin =
    30


init : ( Model, Cmd Msg )
init =
    ( Loading
    , Random.generate GotSeed Random.independentSeed
    )


initFromSeed : Random.Seed -> LoadedModel
initFromSeed seed =
    { devs = [ initDev 2, initDev 1 ]
    , stock = emptyStock
    , seed = seed
    }


updateTime : Float -> Dev -> ( Dev, Stock )
updateTime delta dev =
    case dev.action of
        Nothing ->
            ( dev, emptyStock )

        Just action ->
            let
                newElapsed =
                    action.elapsedMs + delta
            in
            if newElapsed > action.totalMs then
                actionCompleted dev action.kind

            else
                ( { dev | action = Just { action | elapsedMs = newElapsed } }
                , emptyStock
                )


type alias Stock =
    { coffees : List CoffeeId
    , servers : List ServerId
    , apps : List AppId
    , balance : Int
    }


merge : Stock -> Stock -> Stock
merge a1 a2 =
    { coffees = a1.coffees ++ a2.coffees
    , servers = a1.servers ++ a2.servers
    , apps = a1.apps ++ a2.apps
    , balance = a1.balance + a2.balance
    }


emptyStock : Stock
emptyStock =
    { coffees = []
    , servers = []
    , apps = []
    , balance = 0
    }


actionCompleted : Dev -> ActionKind -> ( Dev, Stock )
actionCompleted dev kind =
    let
        newProduced =
            dev.lastProduced + 1

        newDev =
            { dev | lastProduced = newProduced, action = Nothing }
    in
    case kind of
        CreatingCoffee ->
            ( newDev, { emptyStock | coffees = [ CoffeeId dev.id newProduced ] } )

        CreatingServer ->
            ( newDev, { emptyStock | servers = [ ServerId dev.id newProduced ] } )

        CreatingApp coffeeId serverId ->
            ( newDev, { emptyStock | apps = [ AppId dev.id newProduced coffeeId serverId ] } )

        FailingAtCreatingApp _ serverId ->
            ( newDev, { emptyStock | servers = [ serverId ] } )

        SellingApp apps ->
            ( newDev, { emptyStock | balance = appPrice * List.length apps } )


startDevAction : Dev -> ( List Dev, Stock, Random.Seed ) -> ( List Dev, Stock, Random.Seed )
startDevAction dev ( devs, stock, seed ) =
    case dev.action of
        Just _ ->
            -- The dev already is performing an action, so don't change anything!
            ( dev :: devs, stock, seed )

        Nothing ->
            case dev.order of
                CreateCoffee ->
                    ( { dev | action = Just { elapsedMs = 0, totalMs = 1000, kind = CreatingCoffee } } :: devs
                    , stock
                    , seed
                    )

                CreateServer ->
                    let
                        ( time, newSeed ) =
                            Random.step (Random.float 500 2500) seed
                    in
                    ( { dev | action = Just { elapsedMs = 0, totalMs = time, kind = CreatingServer } } :: devs
                    , stock
                    , newSeed
                    )

                CreateApp ->
                    case ( stock.coffees, stock.servers ) of
                        ( coffee :: coffees, server :: servers ) ->
                            let
                                ( success, newSeed ) =
                                    Random.step (Random.weighted ( 0.6, True ) [ ( 0.4, False ) ]) seed

                                action =
                                    if success then
                                        CreatingApp coffee server

                                    else
                                        FailingAtCreatingApp coffee server
                            in
                            ( { dev | action = Just { elapsedMs = 0, totalMs = 2000, kind = action } } :: devs
                            , { stock | coffees = coffees, servers = servers }
                            , newSeed
                            )

                        _ ->
                            ( dev :: devs, stock, seed )

                SellApp ->
                    if not <| List.isEmpty stock.apps then
                        ( { dev | action = Just { elapsedMs = 0, totalMs = 10 * 1000, kind = SellingApp <| List.take maxSellingAppCount stock.apps } } :: devs
                        , { stock | apps = List.drop maxSellingAppCount stock.apps }
                        , seed
                        )

                    else
                        ( dev :: devs, stock, seed )


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
        OrderClicked devId order ->
            ( { model | devs = updateDev devId (\r -> { r | order = order }) model.devs }
            , Cmd.none
            )

        RecruitDevClicked newStock ->
            ( { model
                | stock = newStock
                , devs = addDev model.devs
              }
            , Cmd.none
            )

        GotNewFrame delta ->
            let
                ( tempDevs, tempStock ) =
                    List.map (updateTime delta) model.devs
                        |> List.unzip
                        |> Tuple.mapSecond (List.foldr merge model.stock)

                ( newDevs, newStock, newSeed ) =
                    List.foldr startDevAction ( [], tempStock, model.seed ) tempDevs
            in
            ( { model | devs = newDevs, stock = newStock, seed = newSeed }
            , Cmd.none
            )

        GotSeed seed ->
            -- should not happen!
            ( model
            , Cmd.none
            )


updateDev : DevId -> (Dev -> Dev) -> List Dev -> List Dev
updateDev devId f =
    List.map
        (\dev ->
            if dev.id == devId then
                f dev

            else
                dev
        )


initDev : Int -> Dev
initDev id =
    { id = DevId id
    , order = CreateCoffee
    , action = Nothing
    , lastProduced = 0
    }


addDev : List Dev -> List Dev
addDev devs =
    let
        n =
            List.length devs
    in
    initDev (n + 1) :: devs



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
            if List.length loadedModel.devs >= numberDevToWin then
                H.div [ HA.class "font-bold text-2xl" ]
                    [ H.text "CONGRATS!" ]

            else
                H.div [ HA.class "flex flex-row h-screen min-h-0" ]
                    [ H.div [ HA.class "flex flex-col overflow-y-auto overflow-x-hidden pt-4 pb-8" ]
                        [ viewStock loadedModel.stock
                        , viewDevsCount (List.length loadedModel.devs)
                        , viewRecruitDev loadedModel
                        ]
                    , H.div [ HA.class "flex flex-col overflow-auto flex-grow pt-4" ]
                        [ H.div [ HA.class "flex flex-row flex-wrap pb-8" ]
                            (List.map viewDev (List.reverse loadedModel.devs))
                        ]
                    ]


viewStock : Stock -> Html Msg
viewStock stock =
    H.div [ HA.class "flex flex-col space-y-8" ]
        [ viewResourceCard
            { iconName = "euro_symbol"
            , hardColor = "yellow-700"
            , softColor = "yellow-200"
            , title = "MONEY"
            , value = "€" ++ String.fromInt stock.balance
            }
        , viewResourceCard
            { iconName = "local_cafe"
            , hardColor = "teal-700"
            , softColor = "teal-200"
            , title = "COFFEE"
            , value = String.fromInt (List.length stock.coffees)
            }
        , viewResourceCard
            { iconName = "computer"
            , hardColor = "red-700"
            , softColor = "red-200"
            , title = "SERVER"
            , value = String.fromInt (List.length stock.servers)
            }
        , viewResourceCard
            { iconName = "integration_instructions"
            , hardColor = "green-700"
            , softColor = "green-200"
            , title = "APP"
            , value = String.fromInt (List.length stock.apps)
            }
        ]


viewRecruitDev : LoadedModel -> Html Msg
viewRecruitDev model =
    let
        remainingDevs =
            numberDevToWin - List.length model.devs

        devStr =
            if remainingDevs > 1 then
                "DEVS"

            else
                "DEV"
    in
    H.div [ HA.class "flex flex-col items-center px-4 mt-4" ] <|
        if model.stock.balance >= devMoneyCost && List.length model.stock.coffees >= devCoffeeCost then
            let
                stock =
                    model.stock

                newStock =
                    { stock
                        | balance = stock.balance - devMoneyCost
                        , coffees = List.drop devCoffeeCost stock.coffees
                    }
            in
            [ activeButton "RECRUIT DEV" (RecruitDevClicked newStock)
            , H.span [ HA.class "text-green-500 text-sm invisible" ]
                [ H.text <| "You need €" ++ String.fromInt devMoneyCost ++ " and " ++ String.fromInt devCoffeeCost ++ " coffees."
                ]
            ]

        else
            [ inactiveButton "RECRUIT DEV"
            , H.span [ HA.class "text-green-500 text-sm" ]
                [ H.text <| "You need €" ++ String.fromInt devMoneyCost ++ " and " ++ String.fromInt devCoffeeCost ++ " coffees."
                ]
            ]


viewDevsCount : Int -> Html Msg
viewDevsCount devNumber =
    H.div [ HA.class "flex flex-col items-end leading-none font-bold mt-10 mr-8" ]
        [ H.span [ HA.class "text-gray-500 text-sm" ] [ H.text "DEVS" ]
        , H.span []
            [ H.span [ HA.class "text-green-700 text-4xl" ]
                [ H.text (String.fromInt devNumber) ]
            , H.span [ HA.class "text-xl" ] [ H.text <| "/" ++ String.fromInt numberDevToWin ]
            ]
        ]


activeButton : String -> msg -> Html msg
activeButton text msg =
    H.button
        [ HA.class "rounded-md bg-green-600 px-10 py-2 text-green-200"
        , HE.onClick msg
        ]
        [ H.text text ]


inactiveButton : String -> Html msg
inactiveButton text =
    H.button
        [ HA.class "rounded-md bg-green-100 px-10 py-2 cursor-not-allowed text-green-500"
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
    H.div [ HA.class "h-32 p-4", HA.style "width" "18rem" ]
        [ H.div
            [ HA.class "flex flex-row rounded-md p-8 space-x-8 shadow-lg"

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


viewDev : Dev -> Html Msg
viewDev dev =
    H.div [ HA.class "w-64 px-4 pt-4" ]
        [ H.div [ HA.class "flex flex-col space-y-4 px-4 py-2 border rounded-md h-40 shadow-md" ]
            [ H.h2 [ HA.class "font-bold" ]
                [ viewIcon [ HA.class "mr-4" ] "engineering"
                , H.text <| showDevId dev.id
                ]
            , viewOrders dev
            , case dev.action of
                Nothing ->
                    H.div [ HA.class "flex flex-row justify-center" ]
                        [ viewIcon [] "hourglass_empty" ]

                Just action ->
                    H.div [ HA.class "flex flex-row items-center space-x-4" ]
                        [ viewIcon [] <|
                            case action.kind of
                                CreatingCoffee ->
                                    "local_cafe"

                                CreatingServer ->
                                    "computer"

                                CreatingApp _ _ ->
                                    "integration_instructions"

                                FailingAtCreatingApp _ _ ->
                                    "integration_instructions"

                                SellingApp _ ->
                                    "business_center"
                        , H.div [ HA.class "rounded-full flex-grow h-1 bg-gray-200" ]
                            [ H.div [ HA.class "bg-blue-700 h-1 rounded-full", HA.style "width" <| String.fromFloat (action.elapsedMs / action.totalMs * 100) ++ "%" ]
                                []
                            ]
                        ]
            ]
        ]


viewOrders : Dev -> Html Msg
viewOrders dev =
    H.div [ HA.class "flex flex-row justify-around" ]
        [ orderButton
            { selected = dev.order
            , displayed = CreateCoffee
            }
            dev
        , orderButton
            { selected = dev.order
            , displayed = CreateServer
            }
            dev
        , orderButton
            { selected = dev.order
            , displayed = CreateApp
            }
            dev
        , orderButton
            { selected = dev.order
            , displayed = SellApp
            }
            dev
        ]


orderButton : { selected : Order, displayed : Order } -> Dev -> H.Html Msg
orderButton { selected, displayed } dev =
    (if selected == displayed then
        H.button
            [ HA.class "h-10 w-10 rounded-full bg-blue-200 px-2 py-2 text-blue-700 cursor-not-allowed" ]

     else
        H.button
            [ HA.class "h-10 w-10 rounded-full bg-blue-200 px-2 py-2 text-white"
            , HE.onClick (OrderClicked dev.id displayed)
            ]
    )
    <|
        [ viewIcon [] <|
            case displayed of
                CreateCoffee ->
                    "local_cafe"

                CreateServer ->
                    "computer"

                CreateApp ->
                    "integration_instructions"

                SellApp ->
                    "business_center"
        ]


viewIcon : List (H.Attribute msg) -> String -> Html msg
viewIcon attrs name =
    H.i (HA.class "material-icons" :: attrs) [ H.text name ]


showDevId : DevId -> String
showDevId (DevId id) =
    String.fromInt id


showCoffeeId : CoffeeId -> String
showCoffeeId (CoffeeId devId serialNumber) =
    "C-" ++ showDevId devId ++ "-" ++ String.fromInt serialNumber


showServerId : ServerId -> String
showServerId (ServerId devId serialNumber) =
    "S-" ++ showDevId devId ++ "-" ++ String.fromInt serialNumber


showAppId : AppId -> String
showAppId (AppId devId serialNumber coffeeId serverId) =
    "CS-" ++ showDevId devId ++ "-" ++ String.fromInt serialNumber ++ "-" ++ showCoffeeId coffeeId ++ "-" ++ showServerId serverId


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
