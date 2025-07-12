module CodeEditor exposing (viewCodeEditor)

import Html exposing (..)
import Html.Attributes exposing (class, classList, src, alt)
import Html.Events exposing (onClick)

viewCodeEditor : String -> Html msg
viewCodeEditor activeTab =
    div [ class "code-editor" ]
        [ div [ class "code-header" ]
            [ div [ class "file-tabs" ]
                [ viewFileTab "App.jsx" (activeTab == "App.jsx")
                , viewFileTab "CheckoutForm.jsx" (activeTab == "CheckoutForm.jsx")
                , viewFileTab "Server.js" (activeTab == "Server.js")
                , viewFileTab "package.json" (activeTab == "package.json")
                ]
            , button [ class "download-button" ]
                [ span [ class "download-button-text" ] [ text "Download project" ]
                , img [ src "/images/arrow-down.svg", class "download-button-icon", alt "Download" ] []
                ]
            ]
        , div [ class "code-content" ]
            [ div [ class "line-numbers" ]
                (List.range 1 21
                    |> List.map (\n -> div [] [ text (String.fromInt n) ])
                )
            , div [ class "code-text" ]
                (viewCodeContent activeTab)
            ]
        ]

viewFileTab : String -> Bool -> Html msg
viewFileTab name isActive =
    div [ class "file-tab", classList [ ("active", isActive) ] ]
        [ img [ src "/images/document-icon.svg", class "file-tab-icon", alt "File" ] []
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