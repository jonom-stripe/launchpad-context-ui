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
    | NavigateToOptimalTabOut Page
    | HandleManualNavigationOut Page
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

type alias QuestionInfo =
    { content : String
    , suggestions : List String
    , questionNumber : Int
    , targetPage : Page
    }

type alias Model =
    { messages : List Message
    , inputText : String
    , error : Maybe String
    , contextMenuOpen : Bool
    , suggestionsInline : Bool  -- Track positioning mode
    , suggestionsTransitioning : Bool  -- Track if suggestions are transitioning between positions
    , currentQuestionNumber : Int  -- Track which question we're on (1-5)
    , furthestQuestionReached : Int  -- Track the furthest question the user has reached
    , lastManualNavigation : Maybe Page  -- Track if user manually navigated to a tab
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
    | ClearTransitioning
    | ScrollAfterDelay
    | HandleManualNavigation Page

-- INIT
init : ( Model, Cmd Msg )
init =
    let
        placeholderUserMessage =
            { id = "placeholder-user"
            , content = "I want to build an online platform"
            , timestamp = Time.millisToPosix 0
            , isUser = True
            , visibleChars = 31  -- Length of the message
            , suggestedResponses = []
            , selectedResponse = Nothing
            , visibleResponses = 0
            , removingResponses = False
            }
    in
    ( { messages = [ placeholderUserMessage ]
      , inputText = ""
      , error = Nothing
      , contextMenuOpen = False
      , suggestionsInline = False
      , suggestionsTransitioning = False
      , currentQuestionNumber = 1  -- Start with question 1
      , furthestQuestionReached = 1  -- Start with question 1
      , lastManualNavigation = Nothing  -- No manual navigation yet
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
                
                -- Detect which question is being asked and determine optimal tab
                (newQuestionNumber, optimalTab) = detectQuestionAndTab content model.currentQuestionNumber
                
                -- Update furthest progress if we've advanced
                newFurthestProgress = Basics.max model.furthestQuestionReached newQuestionNumber
                
                -- Add contextual suggestions if this is a response to manual navigation
                contextualSuggestions = case model.lastManualNavigation of
                    Just page ->
                        let
                            sectionName = pageToSectionName page
                        in
                        [ "I'd like to make changes to my " ++ sectionName ++ " setup"
                        , "Let's continue where we left off"
                        ]
                    Nothing ->
                        suggestions
                
                updatedAiMessage = { aiMessage | suggestedResponses = contextualSuggestions }
                
                newModel = 
                    { model 
                    | messages = updatedAiMessage :: model.messages
                    , currentQuestionNumber = newQuestionNumber
                    , furthestQuestionReached = newFurthestProgress
                    , lastManualNavigation = Nothing  -- Clear after handling
                    }
                
                navigationCmd = case optimalTab of
                    Just tab -> NavigateToOptimalTabOut tab
                    Nothing -> NoOut
            in
            ( newModel
            , Task.perform (\_ -> AnimateText aiMessage) (Task.succeed ())
            , navigationCmd
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
            ( { model 
                | messages = updateMessages model.messages
                -- Set transitioning immediately when text completes to prevent flash
                , suggestionsTransitioning = 
                    if isAnimationComplete && not (List.isEmpty updatedMessage.suggestedResponses) then
                        True
                    else
                        model.suggestionsTransitioning
              }
            , if isAnimationComplete then
                -- Text animation complete - immediately measure position before any other actions
                Task.perform (\_ -> RequestPositionAfterDelay) (Task.succeed ())
              else
                Task.perform (\_ -> AnimateText updatedMessage) (Task.succeed ())
            , NoOut
            )

        SelectSuggestedResponse response ->
            let
                -- Check if this is a contextual response we should handle locally
                isContextualResponse = 
                    String.contains "Let's continue where we left off" response ||
                    String.contains "I'd like to make changes to my" response

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

                outMsg = 
                    if isContextualResponse then
                        NoOut  -- Handle locally, don't send to API
                    else
                        NoOut  -- Default case
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
            , outMsg
            )

        AnimateResponses message ->
            ( model
            , if message.visibleResponses < List.length message.suggestedResponses then
                Process.sleep 75  -- Match CSS animation delay (75ms between each suggestion)
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
                
                -- Check if all suggestions are now visible
                allSuggestionsShown = updatedMessage.visibleResponses >= List.length updatedMessage.suggestedResponses
                
                -- Only scroll if suggestions are inline (pinned to message) and all are shown
                shouldScrollAfterSuggestions = allSuggestionsShown && model.suggestionsInline && not model.suggestionsTransitioning
            in
            ( { model | messages = updateMessages model.messages }
            , Cmd.batch
                [ if not allSuggestionsShown then
                    Task.perform (\_ -> AnimateResponses updatedMessage) (Task.succeed ())
                  else
                    Cmd.none
                , if shouldScrollAfterSuggestions then 
                    -- Add small delay to ensure CSS animations complete before scrolling
                    Process.sleep 200
                        |> Task.perform (\_ -> ScrollAfterDelay)
                  else 
                    Cmd.none
                ]
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
                                -- Check if this is a contextual response
                                if String.contains "Let's continue where we left off" response then
                                    -- Handle "continue" locally with proper navigation
                                    let
                                        questionInfo = getQuestionForProgress model.furthestQuestionReached
                                    in
                                    ( Cmd.none
                                    , NavigateToOptimalTabOut questionInfo.targetPage
                                    )
                                else if String.contains "I'd like to make changes to my" response then
                                    -- Handle "make changes" locally
                                    ( Cmd.none
                                    , NoOut
                                    )
                                else
                                    -- Regular response, send to API
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
                            
                            updatedMessagesWithUser = userMessage :: (updateMessages model.messages)
                            
                            -- Add AI follow-up for contextual responses
                            shouldAddFollowUp = 
                                String.contains "Let's continue where we left off" (Maybe.withDefault "" message.selectedResponse) ||
                                String.contains "I'd like to make changes to my" (Maybe.withDefault "" message.selectedResponse)
                        in
                        if shouldAddFollowUp then
                            let
                                selectedResponse = Maybe.withDefault "" message.selectedResponse
                                (questionContent, questionSuggestions) = 
                                    if String.contains "Let's continue where we left off" selectedResponse then
                                        let
                                            questionInfo = getQuestionForProgress model.furthestQuestionReached
                                        in
                                        (questionInfo.content, questionInfo.suggestions)
                                    else
                                        getQuestionForSection selectedResponse
                                
                                aiFollowUp = createNextQuestionMessage questionContent questionSuggestions (List.length updatedMessagesWithUser)
                            in
                            aiFollowUp :: updatedMessagesWithUser
                        else
                            updatedMessagesWithUser
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
                -- Simple decision: negative value means use inline, positive means use input area
                shouldUseInline = gap < 0
                
                -- Check if position mode is changing (or if we're starting from transitioning state)
                isChangingPosition = shouldUseInline /= model.suggestionsInline || model.suggestionsTransitioning
                
                -- Reset suggestion animations since we're determining position
                updatedMessages = 
                    List.map (\message -> 
                        if not message.isUser then
                            { message | visibleResponses = 0, removingResponses = False }
                        else
                            message
                    ) model.messages

                -- Get the latest AI message to start animations
                latestAiMessage = List.head (List.filter (not << .isUser) updatedMessages)
                
                -- Only scroll if there are no suggestions to animate
                shouldScrollNow = case latestAiMessage of
                    Just aiMessage -> List.isEmpty aiMessage.suggestedResponses
                    Nothing -> True
            in
            ( { model 
                | suggestionsInline = shouldUseInline
                , suggestionsTransitioning = True  -- Keep transitioning until animations start
                , messages = updatedMessages
              }
            , Cmd.batch
                [ -- Always start animations after position is determined
                  case latestAiMessage of
                    Just aiMessage ->
                        if not (List.isEmpty aiMessage.suggestedResponses) && 
                           aiMessage.visibleChars == String.length aiMessage.content then
                            Process.sleep 50  -- Short delay to let state update
                                |> Task.perform (\_ -> AnimateResponses aiMessage)
                        else
                            Cmd.none
                    Nothing ->
                        Cmd.none
                , -- Always clear transitioning flag after starting animations
                  Process.sleep 100
                    |> Task.perform (\_ -> ClearTransitioning)
                ]
            , if shouldScrollNow then ScrollToBottomOut else NoOut
            )

        RequestPositionAfterDelay ->
            ( model
            , Cmd.none
            , RequestChatHeightOut
            )

        ClearTransitioning ->
            ( { model | suggestionsTransitioning = False }
            , Cmd.none
            , NoOut
            )

        ScrollAfterDelay ->
            ( model
            , Cmd.none
            , ScrollToBottomOut
            )

        HandleManualNavigation page ->
            let
                pageQuestionNumber = pageToQuestionNumber page
                isGoingBackward = pageQuestionNumber < model.furthestQuestionReached
            in
            if isGoingBackward then
                -- User went back to a previous section, generate contextual message locally
                let
                    sectionName = pageToSectionName page
                    contextualContent = generateContextualMessage page
                    contextualSuggestions = 
                        [ "I'd like to make changes to my " ++ sectionName ++ " setup"
                        , "Let's continue where we left off"
                        ]
                    
                    contextualAiMessage =
                        { id = String.fromInt (List.length model.messages)
                        , content = contextualContent
                        , timestamp = Time.millisToPosix 0
                        , isUser = False
                        , visibleChars = 0  -- Start with no characters visible to animate typing
                        , suggestedResponses = contextualSuggestions
                        , selectedResponse = Nothing
                        , visibleResponses = 0  -- Start with no suggestions visible to animate them in
                        , removingResponses = False
                        }
                in
                ( { model 
                    | messages = contextualAiMessage :: model.messages
                    , lastManualNavigation = Just page
                  }
                , Task.perform (\_ -> AnimateText contextualAiMessage) (Task.succeed ())  -- Start typing animation
                , NoOut
                )
            else
                -- Forward navigation, no special handling needed
                ( { model | lastManualNavigation = Just page }
                , Cmd.none
                , NoOut
                )


-- HELPER FUNCTIONS
detectQuestionAndTab : String -> Int -> (Int, Maybe Page)
detectQuestionAndTab content currentQuestion =
    let
        lowerContent = String.toLower content
    in
    -- Don't navigate for contextual messages asking about changes or continuing
    if String.contains "i see you've returned" lowerContent ||
       String.contains "would you like to make changes" lowerContent ||
       String.contains "shall we continue where we left off" lowerContent then
        (currentQuestion, Nothing)
    else if String.contains "business model" lowerContent then
        (1, Just BusinessModel)
    else if String.contains "collect and pay for fees" lowerContent then
        (2, Just BusinessModel)
    else if String.contains "users to onboard" lowerContent then
        (3, Just Onboarding)
    else if String.contains "buyers pay your sellers" lowerContent then
        (4, Just Checkout)
    else if String.contains "sellers manage their account" lowerContent then
        (5, Just Dashboard)
    else
        -- If no question detected, keep current question number and don't navigate
        (currentQuestion, Nothing)

pageToQuestionNumber : Page -> Int
pageToQuestionNumber page =
    case page of
        BusinessModel -> 1
        Onboarding -> 3
        Checkout -> 4
        Dashboard -> 5
        IntegrationOverview -> 6
        Other -> 0

pageToSectionName : Page -> String
pageToSectionName page =
    case page of
        BusinessModel -> "business model"
        Onboarding -> "onboarding"
        Checkout -> "checkout"
        Dashboard -> "dashboard"
        IntegrationOverview -> "integration overview"
        Other -> "setup"

generateContextualMessage : Page -> String
generateContextualMessage page =
    case page of
        BusinessModel ->
            "I see you've returned to the business model section. Would you like to make changes to your business model setup, or shall we continue where we left off?"
        
        Onboarding ->
            "I see you've returned to the onboarding section. Would you like to make changes to your onboarding setup, or shall we continue where we left off?"
        
        Checkout ->
            "I see you've returned to the checkout section. Would you like to make changes to your checkout setup, or shall we continue where we left off?"
        
        Dashboard ->
            "I see you've returned to the dashboard section. Would you like to make changes to your dashboard setup, or shall we continue where we left off?"
        
        IntegrationOverview ->
            "I see you've returned to the integration overview. Would you like to make changes to your integration setup, or shall we continue where we left off?"
        
        Other ->
            "I see you've navigated to a different section. Would you like to make changes to this setup, or shall we continue where we left off?"

getQuestionForProgress : Int -> QuestionInfo
getQuestionForProgress furthestQuestion =
    case furthestQuestion of
        1 ->
            { content = "What is your business model: a SaaS platform or a Marketplace?"
            , suggestions = [ "SaaS platform", "Marketplace", "I'm not sure" ]
            , questionNumber = 1
            , targetPage = BusinessModel
            }
        2 ->
            { content = "How do you want to collect and pay for fees?"
            , suggestions = [ "Seller pays fees", "You pay fees", "What are the benefits?" ]
            , questionNumber = 2
            , targetPage = BusinessModel
            }
        3 ->
            { content = "How do you want your users to onboard to your platform?"
            , suggestions = [ "With a Stripe-hosted onboarding flow", "With an embedded onboarding flow", "I want to build my own onboarding flow" ]
            , questionNumber = 3
            , targetPage = Onboarding
            }
        4 ->
            { content = "How will buyers pay your sellers: With Stripe-hosted Checkout, embedded components on your site, or with payment links?"
            , suggestions = [ "Use Checkout", "Embed components into my site", "Use payment links" ]
            , questionNumber = 4
            , targetPage = Checkout
            }
        5 ->
            { content = "How will sellers manage their account: with the Stripe Dashboard or embedded components on your site?"
            , suggestions = [ "Stripe Dashboard", "Embedded components", "I'm not sure" ]
            , questionNumber = 5
            , targetPage = Dashboard
            }
        _ ->
            { content = "Great! We've covered all the main questions. Is there anything specific you'd like to review or modify?"
            , suggestions = [ "Review business model", "Review onboarding", "Review checkout", "Review dashboard" ]
            , questionNumber = 5
            , targetPage = Dashboard
            }

getQuestionForSection : String -> (String, List String)
getQuestionForSection response =
    let
        lowerResponse = String.toLower response
    in
    if String.contains "business model" lowerResponse then
        ( "What is your business model: a SaaS platform or a Marketplace?"
        , [ "SaaS platform", "Marketplace", "I'm not sure" ]
        )
    else if String.contains "onboarding" lowerResponse then
        ( "How do you want your users to onboard to your platform?"
        , [ "With a Stripe-hosted onboarding flow", "With an embedded onboarding flow", "I want to build my own onboarding flow" ]
        )
    else if String.contains "checkout" lowerResponse then
        ( "How will buyers pay your sellers: With Stripe-hosted Checkout, embedded components on your site, or with payment links?"
        , [ "Use Checkout", "Embed components into my site", "Use payment links" ]
        )
    else if String.contains "dashboard" lowerResponse then
        ( "How will sellers manage their account: with the Stripe Dashboard or embedded components on your site?"
        , [ "Stripe Dashboard", "Embedded components", "I'm not sure" ]
        )
    else
        ( "I'd be happy to help you modify your setup. Which section would you like to change?"
        , [ "Business model", "Onboarding", "Checkout", "Dashboard" ]
        )

createNextQuestionMessage : String -> List String -> Int -> Message
createNextQuestionMessage content suggestions messageCount =
    { id = String.fromInt messageCount
    , content = content
    , timestamp = Time.millisToPosix 0
    , isUser = False
    , visibleChars = String.length content
    , suggestedResponses = suggestions
    , selectedResponse = Nothing
    , visibleResponses = List.length suggestions
    , removingResponses = False
    }

getSectionQuestionNumber : String -> Int
getSectionQuestionNumber response =
    let
        lowerResponse = String.toLower response
    in
    if String.contains "business model" lowerResponse then
        1
    else if String.contains "onboarding" lowerResponse then
        3
    else if String.contains "checkout" lowerResponse then
        4
    else if String.contains "dashboard" lowerResponse then
        5
    else
        1  -- Default to first question

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
            (List.map (viewMessage 
                (model.suggestionsInline && not model.suggestionsTransitioning) 
                latestAiMessageId) 
                (List.reverse model.messages))
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
            not (List.isEmpty message.suggestedResponses) &&
            not message.removingResponses  -- Don't show if currently being removed
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
            not model.suggestionsTransitioning &&  -- Don't show during transitions
            case latestAiMessage of
                Just message ->
                    -- Show suggestions if message is complete AND we have suggestions AND they're not being removed
                    message.visibleChars == String.length message.content &&
                    message.selectedResponse == Nothing &&
                    not (List.isEmpty message.suggestedResponses) &&
                    not message.removingResponses  -- Don't show if currently being removed
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