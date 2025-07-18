port module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (href, class, classList, src, alt, style, target)
import Url
import Url.Parser as Parser exposing (Parser, (</>), oneOf)
import Workbench
import Chat
import Json.Decode as D
import CodeEditor exposing (viewCodeEditor)
import Task

-- PORTS
port sendMessageToJs : String -> Cmd msg
port messageReceived : (D.Value -> msg) -> Sub msg
port requestSuggestionsPosition : () -> Cmd msg
port suggestionsPositionReceived : (Int -> msg) -> Sub msg
port scrollToBottom : () -> Cmd msg


-- MAIN
main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }

-- MODEL

type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , page : Page
    , chat : Chat.Model
    , codeEditorTab : String
    , hoveredSample : Maybe String
    , showingSourceCode : Bool
    }

type Page
    = Home
    | Code
    | NotFound
    | BusinessModel
    | Onboarding
    | Checkout
    | Dashboard
    | IntegrationOverview

init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        (chatModel, chatCmd) =
            Chat.init
        
        initialPage = urlToPage url
        
        redirectCmd =
            case initialPage of
                Home ->
                    Nav.replaceUrl key "/business-model"
                
                _ ->
                    Cmd.none
    in
    ( { key = key
      , url = url
      , page = initialPage
      , chat = chatModel
      , codeEditorTab = "App.jsx"
      , hoveredSample = Nothing
      , showingSourceCode = False
      }
    , Cmd.batch
        [ Cmd.map ChatMsg chatCmd
        , sendMessageToJs ""  -- Send empty message to get initial response
        , redirectCmd
        ]
    )

-- URL PARSING
urlToPage : Url.Url -> Page
urlToPage url =
    Maybe.withDefault NotFound (Parser.parse routeParser url)

routeParser : Parser (Page -> a) a
routeParser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map Code (Parser.s "code")
        , Parser.map BusinessModel (Parser.s "business-model")
        , Parser.map Onboarding (Parser.s "onboarding")
        , Parser.map Checkout (Parser.s "checkout")
        , Parser.map Dashboard (Parser.s "dashboard")
        , Parser.map IntegrationOverview (Parser.s "integration-overview")
        ]

-- UPDATE
type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url
    | ChatMsg Chat.Msg
    | MessageReceived D.Value
    | CodeEditorTabClicked String
    | SuggestionsPositionReceived Int
    | ManualTabClicked Page
    | ViewSourceClicked
    | SetCodeViewActive Bool

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        UrlChanged url ->
            let
                newPage = urlToPage url
            in
            case newPage of
                Home ->
                    ( { model | url = url, page = newPage }
                    , Nav.replaceUrl model.key "/business-model"
                    )
                
                _ ->
                    ( { model | url = url, page = newPage }
                    , Cmd.none
                    )



        ChatMsg chatMsg ->
            let
                (chatModel, chatCmd, outMsg) =
                    Chat.update chatMsg model.chat
                
                outCmd =
                    case outMsg of
                        Chat.SendMessageOut message ->
                            sendMessageToJs message
                        
                        Chat.NavigateToRootOut ->
                            Nav.pushUrl model.key "/"
                        
                        Chat.NavigateToPageOut page ->
                            Nav.pushUrl model.key (chatPageToUrl page)
                        
                        Chat.RequestChatHeightOut ->
                            requestSuggestionsPosition ()
                        
                        Chat.ScrollToBottomOut ->
                            scrollToBottom ()
                        
                        Chat.NavigateToOptimalTabOut page ->
                            Nav.pushUrl model.key (chatPageToMainPage page)
                        
                        Chat.HandleManualNavigationOut page ->
                            Nav.pushUrl model.key (chatPageToMainPage page)
                        
                        Chat.HoverSuggestedResponseOut response ->
                            Cmd.none
                        
                        Chat.StopHoverSuggestedResponseOut ->
                            Cmd.none
                        
                        Chat.NavigateToPageWithCodeViewOut page ->
                            Cmd.batch
                                [ Nav.pushUrl model.key (chatPageToMainPage page)
                                , Task.perform (\_ -> SetCodeViewActive True) (Task.succeed ())
                                ]
                        
                        Chat.NoOut ->
                            Cmd.none
            in
            let
                updatedModel = case outMsg of
                    Chat.HoverSuggestedResponseOut response ->
                        -- Handle hover for business model responses on the business model page
                        if model.page == BusinessModel then
                            if response == "SaaS platform" then
                                { model | chat = chatModel, hoveredSample = Just "sample-platform" }
                            else if response == "Marketplace" then
                                { model | chat = chatModel, hoveredSample = Just "sample-marketplace" }
                            else
                                { model | chat = chatModel }
                        -- Handle hover for onboarding responses on the onboarding page
                        else if model.page == Onboarding then
                            if String.contains "embedded onboarding flow" (String.toLower response) then
                                { model | chat = chatModel, hoveredSample = Just "sample-onboarding-embedded" }
                            else
                                { model | chat = chatModel, hoveredSample = Nothing }
                        -- Handle hover for dashboard responses on the dashboard page
                        else if model.page == Dashboard then
                            if String.contains "embedded components" (String.toLower response) then
                                { model | chat = chatModel, hoveredSample = Just "sample-dashboard-embedded" }
                            else
                                { model | chat = chatModel, hoveredSample = Nothing }
                        else
                            { model | chat = chatModel }
                    
                    Chat.StopHoverSuggestedResponseOut ->
                        { model | chat = chatModel }  -- Keep current hover state, don't reset
                    
                    _ ->
                        { model | chat = chatModel }
            in
            ( updatedModel
            , Cmd.batch
                [ Cmd.map ChatMsg chatCmd
                , outCmd
                ]
            )

        MessageReceived value ->
            case D.decodeValue messageDecoder value of
                Ok (content, suggestions) ->
                    let
                        (chatModel, chatCmd, outMsg) =
                            Chat.update (Chat.MessageReceived (content, suggestions)) model.chat
                        
                        outCmd =
                            case outMsg of
                                Chat.SendMessageOut message ->
                                    sendMessageToJs message
                                
                                Chat.NavigateToRootOut ->
                                    Nav.pushUrl model.key "/"
                                
                                Chat.NavigateToPageOut page ->
                                    Nav.pushUrl model.key (chatPageToUrl page)
                                
                                Chat.RequestChatHeightOut ->
                                    requestSuggestionsPosition ()
                                
                                Chat.ScrollToBottomOut ->
                                    scrollToBottom ()
                                
                                Chat.NavigateToOptimalTabOut page ->
                                    Nav.pushUrl model.key (chatPageToMainPage page)
                                
                                Chat.HandleManualNavigationOut page ->
                                    Nav.pushUrl model.key (chatPageToMainPage page)
                                
                                Chat.NavigateToPageWithCodeViewOut page ->
                                    Cmd.batch
                                        [ Nav.pushUrl model.key (chatPageToMainPage page)
                                        , Task.perform (\_ -> SetCodeViewActive True) (Task.succeed ())
                                        ]
                                
                                Chat.HoverSuggestedResponseOut response ->
                                    Cmd.none
                                
                                Chat.StopHoverSuggestedResponseOut ->
                                    Cmd.none
                                
                                Chat.NoOut ->
                                    Cmd.none
                    in
                    let
                        updatedModel = case outMsg of
                            Chat.HoverSuggestedResponseOut response ->
                                -- Handle hover for business model responses on the business model page
                                if model.page == BusinessModel then
                                    if response == "SaaS platform" then
                                        { model | chat = chatModel, hoveredSample = Just "sample-platform" }
                                    else if response == "Marketplace" then
                                        { model | chat = chatModel, hoveredSample = Just "sample-marketplace" }
                                    else
                                        { model | chat = chatModel }
                                -- Handle hover for onboarding responses on the onboarding page
                                else if model.page == Onboarding then
                                    if String.contains "embedded onboarding flow" (String.toLower response) then
                                        { model | chat = chatModel, hoveredSample = Just "sample-onboarding-embedded" }
                                    else
                                        { model | chat = chatModel, hoveredSample = Nothing }
                                -- Handle hover for dashboard responses on the dashboard page
                                else if model.page == Dashboard then
                                    if String.contains "embedded components" (String.toLower response) then
                                        { model | chat = chatModel, hoveredSample = Just "sample-dashboard-embedded" }
                                    else
                                        { model | chat = chatModel, hoveredSample = Nothing }
                                else
                                    { model | chat = chatModel }
                            
                            Chat.StopHoverSuggestedResponseOut ->
                                { model | chat = chatModel }  -- Keep current hover state, don't reset
                            
                            _ ->
                                { model | chat = chatModel }
                    in
                    ( updatedModel
                    , Cmd.batch
                        [ Cmd.map ChatMsg chatCmd
                        , outCmd
                        ]
                    )
                
                Err _ ->
                    ( model, Cmd.none )



        CodeEditorTabClicked tabName ->
            ( { model | codeEditorTab = tabName }
            , Cmd.none
            )

        SuggestionsPositionReceived gap ->
            let
                (chatModel, chatCmd, _) =
                    Chat.update (Chat.ChatHeightReceived gap) model.chat
            in
            ( { model | chat = chatModel }
            , Cmd.map ChatMsg chatCmd
            )

        ManualTabClicked page ->
            let
                (chatModel, chatCmd, outMsg) =
                    Chat.update (Chat.HandleManualNavigation (pageToChat page)) model.chat
                
                outCmd =
                    case outMsg of
                        Chat.SendMessageOut message ->
                            sendMessageToJs message
                        
                        Chat.NavigateToRootOut ->
                            Nav.pushUrl model.key "/"
                        
                        Chat.NavigateToPageOut chatPage ->
                            Nav.pushUrl model.key (chatPageToUrl chatPage)
                        
                        Chat.RequestChatHeightOut ->
                            requestSuggestionsPosition ()
                        
                        Chat.ScrollToBottomOut ->
                            scrollToBottom ()
                        
                        Chat.NavigateToOptimalTabOut chatPage ->
                            Nav.pushUrl model.key (chatPageToMainPage chatPage)
                        
                        Chat.HandleManualNavigationOut chatPage ->
                            Nav.pushUrl model.key (chatPageToMainPage chatPage)
                        
                        Chat.HoverSuggestedResponseOut response ->
                            Cmd.none
                        
                        Chat.StopHoverSuggestedResponseOut ->
                            Cmd.none
                        
                        Chat.NavigateToPageWithCodeViewOut chatPage ->
                            Cmd.batch
                                [ Nav.pushUrl model.key (chatPageToMainPage chatPage)
                                , Task.perform (\_ -> SetCodeViewActive True) (Task.succeed ())
                                ]
                        
                        Chat.NoOut ->
                            Cmd.none
            in
            let
                updatedModel = case outMsg of
                    Chat.HoverSuggestedResponseOut response ->
                        -- Handle hover for business model responses on the business model page
                        if page == BusinessModel then
                            if response == "SaaS platform" then
                                { model | chat = chatModel, page = page, hoveredSample = Just "sample-platform", showingSourceCode = False }
                            else if response == "Marketplace" then
                                { model | chat = chatModel, page = page, hoveredSample = Just "sample-marketplace", showingSourceCode = False }
                            else
                                { model | chat = chatModel, page = page, showingSourceCode = False }
                        -- Handle hover for onboarding responses on the onboarding page
                        else if page == Onboarding then
                            if String.contains "embedded onboarding flow" (String.toLower response) then
                                { model | chat = chatModel, page = page, hoveredSample = Just "sample-onboarding-embedded", showingSourceCode = False }
                            else
                                { model | chat = chatModel, page = page, hoveredSample = Nothing, showingSourceCode = False }
                        -- Handle hover for dashboard responses on the dashboard page
                        else if page == Dashboard then
                            if String.contains "embedded components" (String.toLower response) then
                                { model | chat = chatModel, page = page, hoveredSample = Just "sample-dashboard-embedded", showingSourceCode = False }
                            else
                                { model | chat = chatModel, page = page, hoveredSample = Nothing, showingSourceCode = False }
                        else
                            { model | chat = chatModel, page = page, showingSourceCode = False }
                    
                    Chat.StopHoverSuggestedResponseOut ->
                        { model | chat = chatModel, page = page, showingSourceCode = False }  -- Keep current hover state, don't reset
                    
                    _ ->
                        { model | chat = chatModel, page = page, showingSourceCode = False }
            in
            ( updatedModel
            , Cmd.batch
                [ Cmd.map ChatMsg chatCmd
                , outCmd
                ]
            )

        SetCodeViewActive active ->
            ( { model | showingSourceCode = active }, Cmd.none )
        
        ViewSourceClicked ->
            ( { model | showingSourceCode = not model.showingSourceCode }, Cmd.none )

-- HELPERS

pageToChat : Page -> Chat.Page
pageToChat page =
    case page of
        BusinessModel ->
            Chat.BusinessModel
        
        Onboarding ->
            Chat.Onboarding
        
        Checkout ->
            Chat.Checkout
        
        Dashboard ->
            Chat.Dashboard
        
        IntegrationOverview ->
            Chat.IntegrationOverview
        
        _ ->
            Chat.Other

chatPageToUrl : Chat.Page -> String
chatPageToUrl chatPage =
    case chatPage of
        Chat.BusinessModel ->
            "/business-model"
        
        Chat.Onboarding ->
            "/onboarding"
        
        Chat.Checkout ->
            "/checkout"
        
        Chat.Dashboard ->
            "/dashboard"
        
        Chat.IntegrationOverview ->
            "/integration-overview"
        
        Chat.Other ->
            "/"

chatPageToMainPage : Chat.Page -> String
chatPageToMainPage chatPage =
    case chatPage of
        Chat.BusinessModel ->
            "/business-model"
        
        Chat.Onboarding ->
            "/onboarding"
        
        Chat.Checkout ->
            "/checkout"
        
        Chat.Dashboard ->
            "/dashboard"
        
        Chat.IntegrationOverview ->
            "/integration-overview"
        
        Chat.Other ->
            "/"

-- DECODERS

messageDecoder : D.Decoder (String, List String)
messageDecoder =
    D.oneOf
        [ -- Try decoding as an object with content and suggestedResponses
          D.map2 Tuple.pair
            (D.field "content" D.string)
            (D.field "suggestedResponses" (D.list D.string))
        , -- Fall back to decoding as just a string
          D.map (\str -> (str, [])) D.string
        ]





-- VIEW
view : Model -> Browser.Document Msg
view model =
    { title = "Launchpad"
    , body =
        [ div [ class "app" ]
            [ div [ class "columns" ]
                [ div [ class "chat-area" ]
                    [ div []
                        [ Html.map ChatMsg (Chat.view (pageToChat model.page) model.chat)
                        ]
                    ]
                , div [ class "body-area" ]
                    [ div [ class "integrationBrowser" ]
                        [ viewPage model ]
                    ]
                ]
            ]
        ]
    }



viewNavLink : String -> String -> Page -> Page -> Html msg
viewNavLink label icon targetPage currentPage =
    a [ href (pageToUrl targetPage)
      , class "nav-link"
      , classList [ ("active", targetPage == currentPage) ]
      ]
      [ img [ src ("/images/" ++ icon ++ ".svg"), class "nav-link-icon", alt label ] []
      , text label
      ]

pageToUrl : Page -> String
pageToUrl page =
    case page of
        Home ->
            "/"
        
        Code ->
            "/code"
        
        NotFound ->
            "/404"
        
        BusinessModel ->
            "/business-model"
        
        Onboarding ->
            "/onboarding"
        
        Checkout ->
            "/checkout"
        
        Dashboard ->
            "/dashboard"
        
        IntegrationOverview ->
            "/integration-overview"

viewPage : Model -> Html Msg
viewPage model =
    if model.showingSourceCode then
        div [ class "integration-page" ]
            [ viewIntegrationHeader model
            , div [ class "integration-content" ]
                [ viewCode model ]
            , viewIntegrationFooter model.page model.showingSourceCode
            ]
    else
        case model.page of
            Home ->
                div [ class "integration-page" ]
                    [ viewIntegrationHeader model
                    , div [ class "integration-content" ]
                        [ viewHome ]
                    , viewIntegrationFooter model.page model.showingSourceCode
                    ]

            Code ->
                div [ class "integration-page" ]
                    [ viewIntegrationHeader model
                    , div [ class "integration-content" ]
                        [ viewCode model ]
                    , viewIntegrationFooter model.page model.showingSourceCode
                    ]

            BusinessModel ->
                div [ class "integration-page" ]
                    [ viewIntegrationHeader model
                    , div [ class "integration-content business-model-content" ]
                        [ viewBusinessModel model.hoveredSample ]
                    , viewIntegrationFooter model.page model.showingSourceCode
                    ]

            Onboarding ->
                div [ class "integration-page" ]
                    [ viewIntegrationHeader model
                    , div [ class "integration-content" ]
                        [ viewOnboarding model.hoveredSample ]
                    , viewIntegrationFooter model.page model.showingSourceCode
                    ]

            Checkout ->
                div [ class "integration-page" ]
                    [ viewIntegrationHeader model
                    , div [ class "integration-content" ]
                        [ viewCheckout ]
                    , viewIntegrationFooter model.page model.showingSourceCode
                    ]

            Dashboard ->
                div [ class "integration-page" ]
                    [ viewIntegrationHeader model
                    , div [ class "integration-content" ]
                        [ viewDashboard model.hoveredSample ]
                    , viewIntegrationFooter model.page model.showingSourceCode
                    ]

            IntegrationOverview ->
                div [ class "integration-page" ]
                    [ viewIntegrationHeader model
                    , div [ class "integration-content" ]
                        [ viewIntegrationOverview ]
                    , viewIntegrationFooter model.page model.showingSourceCode
                    ]

            NotFound ->
                div [ class "integration-page" ]
                    [ viewIntegrationHeader model
                    , div [ class "integration-content" ]
                        [ text "Page not found" ]
                    , viewIntegrationFooter model.page model.showingSourceCode
                    ]

viewIntegrationHeader : Model -> Html Msg
viewIntegrationHeader model =
    let
        furthestProgress = model.chat.furthestQuestionReached
        
        isShowingCode = model.showingSourceCode
        
        isEmbeddedOnboarding = model.page == Onboarding && model.hoveredSample == Just "sample-onboarding-embedded"
        isEmbeddedDashboard = model.page == Dashboard && model.hoveredSample == Just "sample-dashboard-embedded"
        
        visibleTabs = []
            ++ (if furthestProgress >= 1 then 
                    [ viewIntegrationTab "Business model" "􁽇" (model.page == BusinessModel) BusinessModel False ]
                else 
                    [])
            ++ (if furthestProgress >= 3 then 
                    [ viewIntegrationTab "Onboarding" "􀉭" (model.page == Onboarding) Onboarding isEmbeddedOnboarding ]
                else 
                    [])
            ++ (if furthestProgress >= 4 then 
                    [ viewIntegrationTab "Checkout" "􀍰" (model.page == Checkout) Checkout False ]
                else 
                    [])
            ++ (if furthestProgress >= 5 then 
                    [ viewIntegrationTab "Dashboard" "􂆏" (model.page == Dashboard) Dashboard isEmbeddedDashboard ]
                else 
                    [])
    in
    div [ class "integration-header" ]
        [ div [ class "integration-tabs", classList [ ("integration-tabs-code", isShowingCode) ] ] visibleTabs
        , div [ class "integration-header-space" ] []
        ]

viewIntegrationTab : String -> String -> Bool -> Page -> Bool -> Html Msg
viewIntegrationTab title iconSymbol isActive page isWhiteTheme =
    let
        tabClasses = [ ("active", isActive), ("white-theme", isWhiteTheme) ]
        
        -- Determine background color based on page and theme
        (backgroundColor, textColor) = 
            if isWhiteTheme then
                if page == Onboarding then
                    ("white", "#1a1a1a")
                else if page == Dashboard then
                    ("#f8f9fa", "#1a1a1a")
                else
                    ("white", "#1a1a1a")
            else
                ("", "")
        
        tabStyles = 
            if isWhiteTheme then
                [ style "background-color" backgroundColor
                , style "color" textColor
                , style "border-color" "#e6e6e6"
                ]
            else
                []
        
        iconStyles = 
            if isWhiteTheme then
                [ style "color" textColor ]
            else
                []
        
        textStyles = 
            if isWhiteTheme then
                [ style "color" textColor ]
            else
                []
    in
    div ([ class "integration-tab", classList tabClasses, onClick (ManualTabClicked page) ] ++ tabStyles)
        [ div [ class "integration-tab-content" ]
            [ div ([ class "integration-tab-icon" ] ++ iconStyles)
                [ text iconSymbol ]
            , span ([ class "integration-tab-text" ] ++ textStyles) [ text title ]
            ]
        ]

viewHome : Html msg
viewHome =
    div []
        [ 
        ]

viewBusinessModel : Maybe String -> Html msg
viewBusinessModel hoveredSample =
    let
        imagePath = case hoveredSample of
            Just "sample-marketplace" -> "/images/sample-marketplace.svg"
            Just "sample-platform" -> "/images/sample-platform.svg"
            _ -> "/images/sample-platform.svg"  -- Default to platform
        
        altText = case hoveredSample of
            Just "sample-marketplace" -> "Sample Marketplace"
            _ -> "Sample Platform"
    in
    div [ style "width" "100%", style "height" "100%", style "display" "flex", style "align-items" "center", style "justify-content" "center" ]
        [ img 
            [ src imagePath
            , alt altText
            , style "max-width" "100%"
            , style "max-height" "100%"
            , style "object-fit" "contain"
            , style "transform" "scale(1.25)"
            ]
            []
        ]

viewOnboarding : Maybe String -> Html msg
viewOnboarding hoveredSample =
    let
        imagePath = case hoveredSample of
            Just "sample-onboarding-embedded" -> "/images/sample-onboarding-embedded.png"
            _ -> "/images/sample-onboarding-hosted.png"  -- Default to hosted
        
        altText = case hoveredSample of
            Just "sample-onboarding-embedded" -> "Sample Embedded Onboarding"
            _ -> "Sample Hosted Onboarding"
        
        isEmbedded = hoveredSample == Just "sample-onboarding-embedded"
        
        containerStyles = 
            if isEmbedded then
                [ style "width" "100%"
                , style "min-height" "100%" 
                , style "display" "flex"
                , style "align-items" "flex-start"
                , style "justify-content" "center"
                , style "background-color" "white"
                , style "padding" "20px"
                , style "box-sizing" "border-box"
                ]
            else
                [ style "width" "100%"
                , style "height" "100%" 
                , style "display" "flex"
                , style "flex-direction" "column"
                ]
        
        imageStyles = 
            if isEmbedded then
                [ src imagePath
                , alt altText
                , style "width" "750px"
                , style "max-width" "calc(100% - 40px)"
                , style "height" "auto"
                , style "flex-shrink" "0"
                ]
            else
                [ src imagePath
                , alt altText
                , style "width" "100%"
                , style "height" "auto"
                , style "object-fit" "contain"
                ]
    in
    div containerStyles
        [ img imageStyles []
        ]

viewCheckout : Html msg
viewCheckout =
    div [ style "width" "100%", style "height" "100%", style "display" "flex", style "flex-direction" "column" ]
        [ img 
            [ src "/images/sample-checkout.png"
            , alt "Sample Checkout"
            , style "width" "100%"
            , style "height" "auto"
            , style "object-fit" "contain"
            ]
            []
        ]

viewDashboard : Maybe String -> Html msg
viewDashboard hoveredSample =
    let
        imagePath = case hoveredSample of
            Just "sample-dashboard-embedded" -> "/images/sample-dashboard-embedded.png"
            _ -> "/images/sample-dashboard-stripe.png"  -- Default to stripe
        
        altText = case hoveredSample of
            Just "sample-dashboard-embedded" -> "Sample Embedded Dashboard"
            _ -> "Sample Stripe Dashboard"
        
        isEmbedded = hoveredSample == Just "sample-dashboard-embedded"
        
        containerStyles = 
            if isEmbedded then
                [ style "width" "100%"
                , style "min-height" "100%" 
                , style "display" "flex"
                , style "align-items" "flex-start"
                , style "justify-content" "center"
                , style "background-color" "#f8f9fa"
                , style "padding" "20px"
                , style "box-sizing" "border-box"
                ]
            else
                [ style "width" "100%"
                , style "height" "100%" 
                , style "display" "flex"
                , style "flex-direction" "column"
                ]
        
        imageStyles = 
            if isEmbedded then
                [ src imagePath
                , alt altText
                , style "width" "740px"
                , style "max-width" "calc(100% - 40px)"
                , style "height" "auto"
                , style "flex-shrink" "0"
                ]
            else
                [ src imagePath
                , alt altText
                , style "width" "100%"
                , style "height" "auto"
                , style "object-fit" "contain"
                ]
    in
    div containerStyles
        [ img imageStyles []
        ]

viewIntegrationOverview : Html msg
viewIntegrationOverview =
    div []
        [ 
        ]

viewCode : Model -> Html Msg
viewCode model =
    div [ class "code-editor" ]
        [ div [ class "code-header" ]
            [ div [ class "file-tabs" ]
                [ viewFileTab "App.jsx" (model.codeEditorTab == "App.jsx") (CodeEditorTabClicked "App.jsx")
                , viewFileTab "CheckoutForm.jsx" (model.codeEditorTab == "CheckoutForm.jsx") (CodeEditorTabClicked "CheckoutForm.jsx")
                , viewFileTab "Server.js" (model.codeEditorTab == "Server.js") (CodeEditorTabClicked "Server.js")
                , viewFileTab "package.json" (model.codeEditorTab == "package.json") (CodeEditorTabClicked "package.json")
                ]
            , button [ class "download-button" ]
                [ span [ class "download-button-text" ] [ text "Download project" ]
                , span [ class "download-button-icon" ] [ text "􀈄" ]
                ]
            ]
        , div [ class "code-content" ]
            [ div [ class "line-numbers" ]
                (List.range 1 21
                    |> List.map (\n -> div [] [ text (String.fromInt n) ])
                )
            , div [ class "code-text" ]
                (viewCodeContent model.codeEditorTab)
            ]
        ]

viewFileTab : String -> Bool -> msg -> Html msg
viewFileTab name isActive onClickMsg =
    div [ class "file-tab", classList [ ("active", isActive) ], onClick onClickMsg ]
        [ span [ class "file-tab-icon" ] [ text "􀈷" ]
        , span [ class "file-tab-name" ] [ text name ]
        ]

viewCodeContent : String -> List (Html msg)
viewCodeContent activeTab =
    case activeTab of
        "App.jsx" ->
            [ span [ class "code-keyword" ] [ text "import " ]
            , span [ class "code-module" ] [ text "React " ]
            , span [ class "code-keyword" ] [ text "from " ]
            , span [ class "code-string" ] [ text "\"react\"" ]
            , text ";\n"
            , span [ class "code-keyword" ] [ text "import " ]
            , text "{ "
            , span [ class "code-function" ] [ text "loadStripe" ]
            , text " } "
            , span [ class "code-keyword" ] [ text "from " ]
            , span [ class "code-string" ] [ text "\"@stripe/stripe-js\"" ]
            , text ";\n"
            , span [ class "code-keyword" ] [ text "import " ]
            , text "{ "
            , span [ class "code-component" ] [ text "Elements" ]
            , text " } "
            , span [ class "code-keyword" ] [ text "from " ]
            , span [ class "code-string" ] [ text "\"@stripe/react-stripe-js\"" ]
            , text ";\n"
            , span [ class "code-keyword" ] [ text "import " ]
            , span [ class "code-component" ] [ text "CheckoutForm " ]
            , span [ class "code-keyword" ] [ text "from " ]
            , span [ class "code-string" ] [ text "\"./CheckoutForm\"" ]
            , text ";\n"
            , span [ class "code-keyword" ] [ text "import " ]
            , span [ class "code-string" ] [ text "\"./App.css\"" ]
            , text ";\n\n"
            , span [ class "code-comment" ] [ text "// Make sure to call loadStripe outside of a component's render to avoid\n// recreating the Stripe object on every render.\n// loadStripe is initialized with your publishable key." ]
            , text "\n"
            , span [ class "code-keyword" ] [ text "const " ]
            , span [ class "code-variable" ] [ text "stripePromise " ]
            , text "= "
            , span [ class "code-function" ] [ text "loadStripe" ]
            , text "("
            , span [ class "code-string" ] [ text "\"pk_test_51Nk...\"" ]
            , text ");\n\n"
            , span [ class "code-keyword" ] [ text "export default function " ]
            , span [ class "code-function" ] [ text "App" ]
            , text "() {\n  "
            , span [ class "code-keyword" ] [ text "return " ]
            , text "(\n    <"
            , span [ class "code-component" ] [ text "div " ]
            , span [ class "code-attribute" ] [ text "className" ]
            , text "="
            , span [ class "code-string" ] [ text "\"App\"" ]
            , text ">\n      <"
            , span [ class "code-component" ] [ text "Elements " ]
            , span [ class "code-attribute" ] [ text "stripe" ]
            , text "={"
            , span [ class "code-variable" ] [ text "stripePromise" ]
            , text "}>\n        <"
            , span [ class "code-component" ] [ text "CheckoutForm " ]
            , text "/>\n      </"
            , span [ class "code-component" ] [ text "Elements" ]
            , text ">\n    </"
            , span [ class "code-component" ] [ text "div" ]
            , text ">\n  );\n}"
            ]

        "CheckoutForm.jsx" ->
            [ span [ class "code-keyword" ] [ text "import " ]
            , span [ class "code-module" ] [ text "React, { useState } " ]
            , span [ class "code-keyword" ] [ text "from " ]
            , span [ class "code-string" ] [ text "\"react\"" ]
            , text ";\n"
            , span [ class "code-keyword" ] [ text "import " ]
            , text "{ "
            , span [ class "code-component" ] [ text "CardElement, useStripe, useElements" ]
            , text " } "
            , span [ class "code-keyword" ] [ text "from " ]
            , span [ class "code-string" ] [ text "\"@stripe/react-stripe-js\"" ]
            , text ";\n\n"
            , span [ class "code-keyword" ] [ text "export default function " ]
            , span [ class "code-function" ] [ text "CheckoutForm" ]
            , text "() {\n  "
            , span [ class "code-keyword" ] [ text "const " ]
            , span [ class "code-variable" ] [ text "stripe " ]
            , text "= "
            , span [ class "code-function" ] [ text "useStripe" ]
            , text "();\n  "
            , span [ class "code-keyword" ] [ text "const " ]
            , span [ class "code-variable" ] [ text "elements " ]
            , text "= "
            , span [ class "code-function" ] [ text "useElements" ]
            , text "();\n  "
            , span [ class "code-keyword" ] [ text "const " ]
            , text "["
            , span [ class "code-variable" ] [ text "error" ]
            , text ", "
            , span [ class "code-variable" ] [ text "setError" ]
            , text "] = "
            , span [ class "code-function" ] [ text "useState" ]
            , text "(null);\n\n  "
            , span [ class "code-keyword" ] [ text "const " ]
            , span [ class "code-function" ] [ text "handleSubmit " ]
            , text "= "
            , span [ class "code-keyword" ] [ text "async " ]
            , text "(event) => {\n    "
            , span [ class "code-keyword" ] [ text "event" ]
            , text "."
            , span [ class "code-function" ] [ text "preventDefault" ]
            , text "();\n\n    "
            , span [ class "code-keyword" ] [ text "if " ]
            , text "(!stripe || !elements) {\n      "
            , span [ class "code-keyword" ] [ text "return" ]
            , text ";\n    }\n\n    "
            , span [ class "code-keyword" ] [ text "const " ]
            , span [ class "code-variable" ] [ text "cardElement " ]
            , text "= elements."
            , span [ class "code-function" ] [ text "getElement" ]
            , text "("
            , span [ class "code-component" ] [ text "CardElement" ]
            , text ");\n\n    "
            , span [ class "code-keyword" ] [ text "const " ]
            , span [ class "code-variable" ] [ text "{ error, paymentMethod } " ]
            , text "= "
            , span [ class "code-keyword" ] [ text "await " ]
            , text "stripe."
            , span [ class "code-function" ] [ text "createPaymentMethod" ]
            , text "({\n      "
            , span [ class "code-variable" ] [ text "type" ]
            , text ": "
            , span [ class "code-string" ] [ text "\"card\"" ]
            , text ",\n      "
            , span [ class "code-variable" ] [ text "card" ]
            , text ": cardElement,\n    });\n\n    "
            , span [ class "code-keyword" ] [ text "if " ]
            , text "(error) {\n      "
            , span [ class "code-function" ] [ text "setError" ]
            , text "(error.message);\n    } "
            , span [ class "code-keyword" ] [ text "else " ]
            , text "{\n      "
            , span [ class "code-comment" ] [ text "// Send paymentMethod.id to your server" ]
            , text "\n      "
            , span [ class "code-function" ] [ text "console" ]
            , text "."
            , span [ class "code-function" ] [ text "log" ]
            , text "("
            , span [ class "code-string" ] [ text "\"PaymentMethod:\"" ]
            , text ", paymentMethod);\n    }\n  };\n\n  "
            , span [ class "code-keyword" ] [ text "return " ]
            , text "(\n    <"
            , span [ class "code-component" ] [ text "form " ]
            , span [ class "code-attribute" ] [ text "onSubmit" ]
            , text "={"
            , span [ class "code-variable" ] [ text "handleSubmit" ]
            , text "}>\n      <"
            , span [ class "code-component" ] [ text "CardElement " ]
            , text "/>\n      {error && <"
            , span [ class "code-component" ] [ text "div " ]
            , span [ class "code-attribute" ] [ text "className" ]
            , text "="
            , span [ class "code-string" ] [ text "\"error\"" ]
            , text ">{error}</"
            , span [ class "code-component" ] [ text "div" ]
            , text ">}\n      <"
            , span [ class "code-component" ] [ text "button " ]
            , span [ class "code-attribute" ] [ text "type" ]
            , text "="
            , span [ class "code-string" ] [ text "\"submit\"" ]
            , text " "
            , span [ class "code-attribute" ] [ text "disabled" ]
            , text "={!stripe}>Pay</"
            , span [ class "code-component" ] [ text "button" ]
            , text ">\n    </"
            , span [ class "code-component" ] [ text "form" ]
            , text ">\n  );\n}"
            ]

        "Server.js" ->
            [ span [ class "code-keyword" ] [ text "const " ]
            , span [ class "code-module" ] [ text "express " ]
            , span [ class "code-keyword" ] [ text "= " ]
            , span [ class "code-function" ] [ text "require" ]
            , text "("
            , span [ class "code-string" ] [ text "\"express\"" ]
            , text ");\n"
            , span [ class "code-keyword" ] [ text "const " ]
            , span [ class "code-module" ] [ text "stripe " ]
            , span [ class "code-keyword" ] [ text "= " ]
            , span [ class "code-function" ] [ text "require" ]
            , text "("
            , span [ class "code-string" ] [ text "\"stripe\"" ]
            , text ")("
            , span [ class "code-string" ] [ text "\"sk_test_51Nk...\"" ]
            , text ");\n"
            , span [ class "code-keyword" ] [ text "const " ]
            , span [ class "code-module" ] [ text "app " ]
            , span [ class "code-keyword" ] [ text "= " ]
            , span [ class "code-function" ] [ text "express" ]
            , text "();\n\n"
            , span [ class "code-function" ] [ text "app" ]
            , text "."
            , span [ class "code-function" ] [ text "post" ]
            , text "("
            , span [ class "code-string" ] [ text "\"/create-payment-intent\"" ]
            , text ", "
            , span [ class "code-keyword" ] [ text "async " ]
            , text "(req, res) => {\n  "
            , span [ class "code-keyword" ] [ text "try " ]
            , text "{\n    "
            , span [ class "code-keyword" ] [ text "const " ]
            , span [ class "code-variable" ] [ text "paymentIntent " ]
            , text "= "
            , span [ class "code-keyword" ] [ text "await " ]
            , text "stripe."
            , span [ class "code-function" ] [ text "paymentIntents" ]
            , text "."
            , span [ class "code-function" ] [ text "create" ]
            , text "({\n      "
            , span [ class "code-variable" ] [ text "amount" ]
            , text ": 2000,\n      "
            , span [ class "code-variable" ] [ text "currency" ]
            , text ": "
            , span [ class "code-string" ] [ text "\"usd\"" ]
            , text ",\n    });\n\n    "
            , span [ class "code-function" ] [ text "res" ]
            , text "."
            , span [ class "code-function" ] [ text "send" ]
            , text "({\n      "
            , span [ class "code-variable" ] [ text "clientSecret" ]
            , text ": paymentIntent."
            , span [ class "code-variable" ] [ text "client_secret" ]
            , text ",\n    });\n  } "
            , span [ class "code-keyword" ] [ text "catch " ]
            , text "(error) {\n    "
            , span [ class "code-function" ] [ text "res" ]
            , text "."
            , span [ class "code-function" ] [ text "status" ]
            , text "(500)."
            , span [ class "code-function" ] [ text "send" ]
            , text "({ "
            , span [ class "code-variable" ] [ text "error" ]
            , text ": error."
            , span [ class "code-variable" ] [ text "message" ]
            , text " });\n  }\n});\n\n"
            , span [ class "code-function" ] [ text "app" ]
            , text "."
            , span [ class "code-function" ] [ text "listen" ]
            , text "(3000, () => "
            , span [ class "code-function" ] [ text "console" ]
            , text "."
            , span [ class "code-function" ] [ text "log" ]
            , text "("
            , span [ class "code-string" ] [ text "\"Server running on port 3000\"" ]
            , text "));"
            ]

        "package.json" ->
            [ text "{\n  "
            , span [ class "code-string" ] [ text "\"name\"" ]
            , text ": "
            , span [ class "code-string" ] [ text "\"stripe-payment-demo\"" ]
            , text ",\n  "
            , span [ class "code-string" ] [ text "\"version\"" ]
            , text ": "
            , span [ class "code-string" ] [ text "\"1.0.0\"" ]
            , text ",\n  "
            , span [ class "code-string" ] [ text "\"dependencies\"" ]
            , text ": {\n    "
            , span [ class "code-string" ] [ text "\"@stripe/stripe-js\"" ]
            , text ": "
            , span [ class "code-string" ] [ text "\"^2.0.0\"" ]
            , text ",\n    "
            , span [ class "code-string" ] [ text "\"@stripe/react-stripe-js\"" ]
            , text ": "
            , span [ class "code-string" ] [ text "\"^2.0.0\"" ]
            , text ",\n    "
            , span [ class "code-string" ] [ text "\"express\"" ]
            , text ": "
            , span [ class "code-string" ] [ text "\"^4.18.2\"" ]
            , text ",\n    "
            , span [ class "code-string" ] [ text "\"react\"" ]
            , text ": "
            , span [ class "code-string" ] [ text "\"^18.2.0\"" ]
            , text ",\n    "
            , span [ class "code-string" ] [ text "\"react-dom\"" ]
            , text ": "
            , span [ class "code-string" ] [ text "\"^18.2.0\"" ]
            , text ",\n    "
            , span [ class "code-string" ] [ text "\"stripe\"" ]
            , text ": "
            , span [ class "code-string" ] [ text "\"^14.0.0\"" ]
            , text "\n  }\n}"
            ]

        _ ->
            []



viewNotFound : Html msg
viewNotFound =
    div []
        [ h1 [] [ text "Not Found" ]
        , p [] [ text "Page not found." ]
        ]

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ messageReceived MessageReceived
        , suggestionsPositionReceived SuggestionsPositionReceived
        ]

viewIntegrationFooter : Page -> Bool -> Html Msg
viewIntegrationFooter currentPage showingSourceCode =
    let
        shouldHideViewSource = 
            currentPage == BusinessModel || currentPage == Dashboard
        
        buttonText = if showingSourceCode then "Show UI" else "View source"
    in
    div [ class "integration-footer" ]
        [ div [ class "footer-actions" ]
            (if shouldHideViewSource then
                []
            else
                [ button 
                    [ class "footer-button"
                    , onClick ViewSourceClicked
                    ]
                    [ div [ class "footer-button-icon" ]
                        [ text "􀪏" ]
                    , span [ class "footer-button-text" ] [ text buttonText ]
                    ]
                ]
            )
        ]

