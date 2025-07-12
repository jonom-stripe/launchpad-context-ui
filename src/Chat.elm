module Chat exposing 
    ( Model
    , Msg(..)
    , init
    , update
    , view
    , viewInput
    , OutMsg(..), viewError
    , Page(..)
    , subscriptions
    )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onSubmit, onInput, onClick, stopPropagationOn)
import Time
import Json.Decode as D
import Process
import Task
import Svg
import Svg.Attributes

-- TYPES
type OutMsg
    = SendMessageOut String
    | NavigateToRootOut
    | NavigateToPageOut Page
    | RequestChatHeightOut
    | ScrollToBottomOut
    | NoOut

type Page
    = BusinessModel
    | Onboarding
    | Checkout
    | Dashboard
    | IntegrationOverview
    | Other

type alias Message =
    { id : String
    , content : String
    , timestamp : Time.Posix
    , isUser : Bool
    , visibleChars : Int
    , suggestedResponses : List String
    , selectedResponse : Maybe String
    , visibleResponses : Int
    , removingResponses : Bool
    }

type alias Model =
    { messages : List Message
    , inputText : String
    , error : Maybe String
    , contextMenuOpen : Bool
    , suggestionsInline : Bool  -- New field to track positioning mode
    }

type Msg
    = InputChanged String
    | SendMessage
    | MessageReceived (String, List String)
    | GotError String
    | AnimateText Message
    | ShowNextChar Message
    | SelectSuggestedResponse String
    | AnimateResponses Message
    | ShowNextResponse Message
    | RemoveResponses Message
    | UpdateMessage Message
    | NavigateToRoot
    | ToggleContextMenu
    | CloseContextMenu
    | SelectContext Page
    | ChatHeightReceived Int
    | RequestPositionAfterDelay
    | ScrollAndRequestPosition

-- INIT
init : ( Model, Cmd Msg )
init =
    ( { messages = []
      , inputText = ""
      , error = Nothing
      , contextMenuOpen = False
      , suggestionsInline = False
      }
    , Cmd.none
    )

-- UPDATE
update : Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update msg model =
    case msg of
        NavigateToRoot ->
            ( model
            , Cmd.none
            , NavigateToRootOut
            )

        InputChanged text ->
            ( { model | inputText = text }
            , Cmd.none
            , NoOut
            )

        SendMessage ->
            if String.isEmpty model.inputText then
                ( model, Cmd.none, NoOut )
            else
                let
                    aiMessage =
                        List.head (List.filter (not << .isUser) model.messages)
                        
                    updateMessage message =
                        if not message.isUser then
                            { message 
                            | removingResponses = True
                            , visibleResponses = 
                                if message.visibleResponses > 0 then
                                    List.length message.suggestedResponses
                                else 
                                    0
                            }
                        else
                            message

                    userMessage =
                        { id = String.fromInt (List.length model.messages)
                        , content = model.inputText
                        , timestamp = Time.millisToPosix 0
                        , isUser = True
                        , visibleChars = String.length model.inputText
                        , suggestedResponses = []
                        , selectedResponse = Nothing
                        , visibleResponses = 0
                        , removingResponses = False
                        }

                    updatedMessages =
                        userMessage :: List.map updateMessage model.messages
                in
                ( { model 
                    | messages = updatedMessages
                    , inputText = ""
                  }
                , case aiMessage of
                    Just message ->
                        Process.sleep 50
                            |> Task.perform (\_ -> RemoveResponses { message | visibleResponses = List.length message.suggestedResponses })
                    Nothing ->
                        Cmd.none
                , SendMessageOut model.inputText
                )

        MessageReceived (content, suggestions) ->
            let
                aiMessage =
                    { id = String.fromInt (List.length model.messages)
                    , content = content
                    , timestamp = Time.millisToPosix 0
                    , isUser = False
                    , visibleChars = 0
                    , suggestedResponses = suggestions
                    , selectedResponse = Nothing
                    , visibleResponses = 0
                    , removingResponses = False
                    }
                
                newModel = { model | messages = aiMessage :: model.messages }
            in
            ( newModel
            , Task.perform (\_ -> AnimateText aiMessage) (Task.succeed ())
            , NoOut
            )

        GotError error ->
            ( { model | error = Just error }
            , Cmd.none
            , NoOut
            )

        AnimateText message ->
            ( model
            , if message.visibleChars < String.length message.content then
                Process.sleep 5
                    |> Task.perform (\_ -> ShowNextChar message)
              else
                Cmd.none
            , NoOut
            )

        ShowNextChar message ->
            let
                updatedMessage =
                    { message | visibleChars = message.visibleChars + 1 }
                
                updateMessages msgs =
                    case msgs of
                        [] ->
                            []
                        
                        m :: rest ->
                            if m.id == message.id then
                                updatedMessage :: rest
                            else
                                m :: updateMessages rest
                
                isAnimationComplete = updatedMessage.visibleChars >= String.length message.content
            in
            ( { model | messages = updateMessages model.messages }
            , if isAnimationComplete then
                -- Animation complete, scroll and then request position measurement after small delay
                Process.sleep 100
                    |> Task.perform (\_ -> ScrollAndRequestPosition)
              else
                Task.perform (\_ -> AnimateText updatedMessage) (Task.succeed ())
            , NoOut
            )

        SelectSuggestedResponse response ->
            let
                updateMessage message =
                    if not message.isUser then
                        { message 
                        | removingResponses = True
                        , selectedResponse = Just response
                        , visibleResponses = 
                            if message.visibleResponses > 0 then
                                List.length message.suggestedResponses
                            else 
                                0
                        }
                    else
                        message

                updatedMessages =
                    List.map updateMessage model.messages

                aiMessage =
                    List.head (List.filter (not << .isUser) model.messages)
            in
            ( { model | messages = updatedMessages }
            , case aiMessage of
                Just foundMessage ->
                    Process.sleep 50
                        |> Task.perform (\_ -> RemoveResponses 
                            { foundMessage 
                            | selectedResponse = Just response
                            , visibleResponses = List.length foundMessage.suggestedResponses 
                            })
                Nothing ->
                    Cmd.none
            , NoOut
            )

        AnimateResponses message ->
            ( model
            , if message.visibleResponses < List.length message.suggestedResponses then
                Process.sleep 5
                    |> Task.perform (\_ -> ShowNextResponse message)
              else
                Cmd.none
            , NoOut
            )

        ShowNextResponse message ->
            let
                updatedMessage =
                    { message | visibleResponses = message.visibleResponses + 1 }
                
                updateMessages msgs =
                    case msgs of
                        [] ->
                            []
                        
                        m :: rest ->
                            if m.id == message.id then
                                updatedMessage :: rest
                            else
                                m :: updateMessages rest
            in
            ( { model | messages = updateMessages model.messages }
            , if updatedMessage.visibleResponses < List.length updatedMessage.suggestedResponses then
                Cmd.none
              else
                Cmd.none
            , NoOut
            )

        RemoveResponses message ->
            let
                updatedMessage =
                    { message | visibleResponses = message.visibleResponses - 1 }
                
                updateMessages msgs =
                    case msgs of
                        [] ->
                            []
                        
                        m :: rest ->
                            if m.id == message.id then
                                updatedMessage :: rest
                            else
                                m :: updateMessages rest

                (nextCmd, outMsg) =
                    if updatedMessage.visibleResponses > 0 then
                        ( Process.sleep 50
                            |> Task.perform (\_ -> RemoveResponses updatedMessage)
                        , NoOut
                        )
                    else
                        case message.selectedResponse of
                            Just response ->
                                -- Only send message if this was a selected response
                                ( Cmd.none
                                , SendMessageOut response
                                )
                            Nothing ->
                                ( Cmd.none
                                , NoOut
                                )
            in
            ( { model 
                | messages = 
                    if updatedMessage.visibleResponses <= 0 && message.selectedResponse /= Nothing then
                        let
                            userMessage =
                                { id = String.fromInt (List.length model.messages)
                                , content = Maybe.withDefault "" message.selectedResponse
                                , timestamp = Time.millisToPosix 0
                                , isUser = True
                                , visibleChars = String.length (Maybe.withDefault "" message.selectedResponse)
                                , suggestedResponses = []
                                , selectedResponse = Nothing
                                , visibleResponses = 0
                                , removingResponses = False
                                }
                        in
                        userMessage :: (updateMessages model.messages)
                    else
                        updateMessages model.messages
              }
            , nextCmd
            , outMsg
            )

        UpdateMessage message ->
            let
                updateMessages msgs =
                    case msgs of
                        [] ->
                            []
                        
                        m :: rest ->
                            if m.id == message.id then
                                message :: rest
                            else
                                m :: updateMessages rest
            in
            ( { model | messages = updateMessages model.messages }
            , Cmd.none
            , NoOut
            )

        ToggleContextMenu ->
            ( { model | contextMenuOpen = not model.contextMenuOpen }
            , Cmd.none
            , NoOut
            )

        CloseContextMenu ->
            ( { model | contextMenuOpen = False }
            , Cmd.none
            , NoOut
            )

        SelectContext page ->
            ( { model | contextMenuOpen = False }
            , Cmd.none
            , NavigateToPageOut page
            )

        ChatHeightReceived gap ->
            let
                -- Determine if suggestions should be inline based on gap between messages and suggestions
                -- If the gap is less than 50px, the messages are getting too close to the suggestions
                -- and we should move suggestions inline to avoid collision
                shouldUseInline = gap < 50
            in
            ( { model | suggestionsInline = shouldUseInline }
            , Cmd.none
            , NoOut
            )

        RequestPositionAfterDelay ->
            ( model
            , Cmd.none
            , RequestChatHeightOut
            )

        ScrollAndRequestPosition ->
            ( model
            , Cmd.none
            , ScrollToBottomOut
            )



-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

-- VIEW
view : Page -> Model -> Html Msg
view page model =
    let
        latestAiMessage = List.head (List.filter (not << .isUser) model.messages)
        latestAiMessageId = Maybe.map .id latestAiMessage
    in
    div [ class "chat-container" ]
        [ viewHeader
        , div [ class "messages-container" ]
            (List.map (viewMessage model.suggestionsInline latestAiMessageId) (List.reverse model.messages))
        , viewInput page model
        , viewError model.error
        ]

contextTextForPage : Page -> String
contextTextForPage page =
    case page of
        BusinessModel ->
            "Set up business model"
        
        Onboarding ->
            "Set up onboarding"
        
        Checkout ->
            "Set up checkout"
        
        Dashboard ->
            "Set up dashboard"
        
        IntegrationOverview ->
            "Integration overview"
        
        Other ->
            "Edit integration type"

onClickNoPropagate : msg -> Html.Attribute msg
onClickNoPropagate msg =
    stopPropagationOn "click" (D.succeed (msg, True))

viewContextItem : Page -> Page -> Html Msg
viewContextItem currentPage itemPage =
    div [ class "context-picker-item"
        , classList [ ("context-picker-item-selected", currentPage == itemPage) ]
        , onClickNoPropagate (SelectContext itemPage)
        ]
        [ text (contextTextForPage itemPage) ]

viewHeader : Html Msg
viewHeader =
    div [ class "chat-header" ]
        [ div 
            [ class "header-content"
            , Html.Events.onClick NavigateToRoot
            , style "cursor" "pointer"
            ]
            [ div [ class "back-arrow" ]
                [ Svg.svg 
                    [ Svg.Attributes.width "24"
                    , Svg.Attributes.height "24"
                    , Svg.Attributes.viewBox "0 0 24 24"
                    , Svg.Attributes.fill "none"
                    ]
                    [ Svg.path
                        [ Svg.Attributes.d "M12 18L6 12L12 6M6 12H18"
                        , Svg.Attributes.stroke "#474E5A"
                        , Svg.Attributes.strokeWidth "2"
                        , Svg.Attributes.strokeLinecap "round"
                        , Svg.Attributes.strokeLinejoin "round"
                        ]
                        []
                    ]
                ]
            , div [ class "header-labels" ]
                [ span [ class "header-subtitle" ] [ text "Stripe Docs" ]
                , span [ class "header-title" ] [ text "Launchpad" ]
                ]
            ]
        ]

viewMessage : Bool -> Maybe String -> Message -> Html Msg
viewMessage suggestionsInline latestAiMessageId message =
    let
        isLatestAiMessage = case latestAiMessageId of
            Just id -> id == message.id
            Nothing -> False
        shouldShowInlineSuggestions = 
            suggestionsInline && 
            isLatestAiMessage &&
            not message.isUser && 
            message.visibleChars == String.length message.content && 
            message.selectedResponse == Nothing &&
            not (List.isEmpty message.suggestedResponses)
    in
    div [ class "message-container" ]
        [ div
            [ class "message"
            , classList
                [ ( "message-user", message.isUser )
                , ( "message-other", not message.isUser )
                ]
            ]
            [ div [ class "message-content" ] 
                [ if message.isUser then
                    text message.content
                  else
                    span []
                        [ span [ class "typing-animation"
                              , classList [ ("visible", True) ]
                              ] 
                              [ text (String.left message.visibleChars message.content) ]
                        , if message.visibleChars < String.length message.content then
                            span [ class "typing-indicator" ]
                                [ span [ class "typing-dot" ] []
                                ]
                          else
                            text ""
                        ]
                ]
            ]
        , if shouldShowInlineSuggestions then
            viewSuggestedResponses message.suggestedResponses message.selectedResponse message
          else
            text ""
        ]

viewMessageList : List Message -> Html Msg
viewMessageList messages =
    let
        latestAiMessage = List.head (List.filter (not << .isUser) messages)
        latestAiMessageId = Maybe.map .id latestAiMessage
    in
    div [ class "message-list" ]
        (List.map (viewMessage False latestAiMessageId) (List.reverse messages))

viewInputArea : Model -> Html Msg
viewInputArea model =
    let
        latestAiMessage = 
            List.head (List.filter (not << .isUser) model.messages)
        
        shouldShowSuggestions =
            case latestAiMessage of
                Just message ->
                    message.visibleChars == String.length message.content && 
                    message.selectedResponse == Nothing
                Nothing ->
                    False
    in
    div [ class "chat-input-container" ]
        [ case latestAiMessage of
            Just message ->
                if shouldShowSuggestions then
                    viewSuggestedResponses message.suggestedResponses message.selectedResponse message
                else
                    text ""
            Nothing ->
                text ""
        , div [ class "chat-input-wrapper" ]
            [ Html.form [onSubmit SendMessage ]
                [ textarea
                    [ placeholder "Ask a question..."
                    , value model.inputText
                    , onInput InputChanged
                    , class "chat-input"
                    ]
                    []
                ]
            , div
                [ class "context-picker-button" ]
                [ div [ class "context-picker-collapsed" ]
                    [ text "Select context" ]
                , div [ class "context-picker-expanded" ]
                    [ div [ class "context-picker-item context-picker-item-selected" ]
                        [ text "Change integration type" ]
                    , div [ class "context-picker-item" ]
                        [ text "Manage demo products" ]
                    , div [ class "context-picker-item" ]
                        [ text "Review sample code" ]
                    , div [ class "context-picker-item" ]
                        [ text "Setup VS Code extension" ]
                    , div [ class "context-picker-item" ]
                        [ text "Test integration" ]
                    , div [ class "context-picker-item" ]
                        [ text "Go live" ]
                    , div [ class "context-picker-item context-picker-item-muted" ]
                     [ text "Search..." ]
                    ]
                ]
            ]
        ]
    

viewSuggestedResponses : List String -> Maybe String -> Message -> Html Msg
viewSuggestedResponses responses selectedResponse message =
    div 
        [ class "suggested-responses"
        , Html.Attributes.id "latest-suggestions"
        ]
        (List.indexedMap (viewSuggestedResponse message) responses)

viewSuggestedResponse : Message -> Int -> String -> Html Msg
viewSuggestedResponse message index response =
    button 
        [ class "suggested-response"
        , classList 
            [ ("fade-in", not message.removingResponses)
            , ("fade-out", message.removingResponses)
            ]
        , style "animation-delay" 
            (if message.removingResponses then
                String.fromFloat (toFloat (List.length message.suggestedResponses - index - 1) * 0.075) ++ "s"
             else
                String.fromFloat (toFloat index * 0.075) ++ "s"
            )
        , style "animation-duration" "0.15s"
        , Html.Events.onClick (SelectSuggestedResponse response)
        ]
        [ text response ]

viewError : Maybe String -> Html Msg
viewError maybeError =
    case maybeError of
        Just error ->
            div [ class "error" ] [ text error ]

        Nothing ->
            text ""

viewInput : Page -> Model -> Html Msg
viewInput page model =
    let
        latestAiMessage = 
            List.head (List.filter (not << .isUser) model.messages)
        
        shouldShowSuggestions =
            not model.suggestionsInline &&  -- Only show here when not inline
            case latestAiMessage of
                Just message ->
                    -- Show suggestions if message is complete OR if this is the first message
                    (message.visibleChars == String.length message.content || List.length model.messages == 1) &&
                    message.selectedResponse == Nothing &&
                    not (List.isEmpty message.suggestedResponses)
                Nothing ->
                    False
        
        -- Get suggestions to show (from latest AI message)
        suggestionsToShow =
            case latestAiMessage of
                Just message ->
                    if shouldShowSuggestions then
                        message.suggestedResponses
                    else
                        []
                Nothing ->
                    []
    in
    Html.form [ onSubmit SendMessage, class "chat-input-container" ]
        [ if not (List.isEmpty suggestionsToShow) then
            case latestAiMessage of
                Just message ->
                    viewSuggestedResponses suggestionsToShow message.selectedResponse message
                Nothing ->
                    text ""
          else
            text ""
        , if model.contextMenuOpen then
            div [ class "context-menu-backdrop", onClick CloseContextMenu ] []
          else
            text ""
        , div [ class "chat-input-wrapper" ]
            [ textarea
                [ value model.inputText
                , onInput InputChanged
                , placeholder "Ask a question..."
                , class "chat-input"
                ]
                []
            , div
                [ class "context-picker-button"
                , classList [("context-menu-open", model.contextMenuOpen)]
                , onClick ToggleContextMenu
                ]
                [ if not model.contextMenuOpen then
                    div [ class "context-picker-collapsed" ]
                        [ text (contextTextForPage page) ]
                  else
                    text ""
                , if model.contextMenuOpen then
                    div [ class "context-picker-expanded" ]
                        [ viewContextItem page BusinessModel
                        , viewContextItem page Onboarding
                        , viewContextItem page Checkout
                        , viewContextItem page Dashboard
                        , viewContextItem page IntegrationOverview
                        ]
                  else
                    text ""
                ]
            ]
        ]

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