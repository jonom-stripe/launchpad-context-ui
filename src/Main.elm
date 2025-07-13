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
                        
                        Chat.NoOut ->
                            Cmd.none
            in
            ( { model | chat = chatModel }
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
                                
                                Chat.NoOut ->
                                    Cmd.none
                    in
                    ( { model | chat = chatModel }
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
                        
                        Chat.NoOut ->
                            Cmd.none
            in
            ( { model | chat = chatModel, page = page }
            , Cmd.batch
                [ Cmd.map ChatMsg chatCmd
                , outCmd
                ]
            )

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
    case model.page of
        Home ->
            div [ class "integration-page" ]
                [ viewIntegrationHeader model
                , div [ class "integration-content" ]
                    [ viewHome ]
                , viewIntegrationFooter
                ]

        Code ->
            div [ class "integration-page" ]
                [ viewIntegrationHeader model
                , div [ class "integration-content" ]
                    [ viewCode model ]
                , viewIntegrationFooter
                ]

        BusinessModel ->
            div [ class "integration-page" ]
                [ viewIntegrationHeader model
                , div [ class "integration-content business-model-content" ]
                    [ viewBusinessModel ]
                , viewIntegrationFooter
                ]

        Onboarding ->
            div [ class "integration-page" ]
                [ viewIntegrationHeader model
                , div [ class "integration-content" ]
                    [ viewOnboarding ]
                , viewIntegrationFooter
                ]

        Checkout ->
            div [ class "integration-page" ]
                [ viewIntegrationHeader model
                , div [ class "integration-content" ]
                    [ viewCheckout ]
                , viewIntegrationFooter
                ]

        Dashboard ->
            div [ class "integration-page" ]
                [ viewIntegrationHeader model
                , div [ class "integration-content" ]
                    [ viewDashboard ]
                , viewIntegrationFooter
                ]

        IntegrationOverview ->
            div [ class "integration-page" ]
                [ viewIntegrationHeader model
                , div [ class "integration-content" ]
                    [ viewIntegrationOverview ]
                , viewIntegrationFooter
                ]

        NotFound ->
            viewNotFound

viewIntegrationHeader : Model -> Html Msg
viewIntegrationHeader model =
    div [ class "integration-header" ]
        [ div [ class "integration-tabs" ]
            [ viewIntegrationTab "Business model" "􁽇" (model.page == BusinessModel) BusinessModel
            , viewIntegrationTab "Onboarding" "􀉭" (model.page == Onboarding) Onboarding
            , viewIntegrationTab "Checkout" "􀍰" (model.page == Checkout) Checkout
            , viewIntegrationTab "Dashboard" "􂆏" (model.page == Dashboard) Dashboard
            -- , viewIntegrationTab "Integration overview" "📄" (model.page == IntegrationOverview) IntegrationOverview
            ]
        , div [ class "integration-header-space" ] []
        ]

viewIntegrationTab : String -> String -> Bool -> Page -> Html Msg
viewIntegrationTab title iconSymbol isActive page =
    div [ class "integration-tab", classList [ ("active", isActive) ], onClick (ManualTabClicked page) ]
        [ div [ class "integration-tab-content" ]
            [ div [ class "integration-tab-icon" ]
                [ text iconSymbol ]
            , span [ class "integration-tab-text" ] [ text title ]
            ]
        ]

viewHome : Html msg
viewHome =
    div []
        [ 
        ]

viewBusinessModel : Html msg
viewBusinessModel =
    div [ style "width" "100%", style "height" "100%", style "display" "flex", style "align-items" "center", style "justify-content" "center" ]
        [ img 
            [ src "/images/sample-platform.svg"
            , alt "Sample Platform"
            , style "max-width" "100%"
            , style "max-height" "100%"
            , style "object-fit" "contain"
            , style "transform" "scale(1.25)"
            ]
            []
        ]

viewOnboarding : Html msg
viewOnboarding =
    div [ style "width" "100%", style "height" "100%", style "display" "flex", style "flex-direction" "column" ]
        [ img 
            [ src "/images/sample-onboarding.png"
            , alt "Sample Onboarding"
            , style "width" "100%"
            , style "height" "auto"
            , style "object-fit" "contain"
            ]
            []
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

viewDashboard : Html msg
viewDashboard =
    div [ style "width" "100%", style "height" "100%", style "display" "flex", style "flex-direction" "column" ]
        [ img 
            [ src "/images/sample-dashboard.png"
            , alt "Sample Dashboard"
            , style "width" "100%"
            , style "height" "auto"
            , style "object-fit" "contain"
            ]
            []
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

viewIntegrationFooter : Html Msg
viewIntegrationFooter =
    div [ class "integration-footer" ]
        [ div [ class "footer-actions" ]
            [ button [ class "footer-button" ]
                [ div [ class "footer-button-icon" ]
                    [ text "􀍟" ]
                , span [ class "footer-button-text" ] [ text "Edit" ]
                ]
            , button [ class "footer-button footer-button-disabled" ]
                [ div [ class "footer-button-icon" ]
                    [ text "􀪏" ]
                , span [ class "footer-button-text" ] [ text "View source" ]
                ]
            ]
        ]

