# Launchpad

A modern chat interface built with Elm, featuring an innovative text selection-based interaction model.

## Domain Model

### Core Concepts

#### Selection Context
- **Selected Text**: The text highlighted by the user that serves as context for the interaction
- **Selection Range**: The start and end points of the selected text within the document
- **Selection Bounds**: The physical coordinates and dimensions of the selected text on screen

#### Chat Elements
- **Message**: A single chat entry, either from the user or AI
  - **User Message**: Text input submitted by the user
  - **AI Message**: Response from the AI, displayed with a typing animation
- **Message Stream**: The continuous flow of text as the AI response is generated
- **Suggested Responses**: Quick-action buttons that appear after AI messages with contextual suggestions

#### UI Components
- **Selection Popup**: The floating menu that appears when text is selected
  - **Popup Anchor**: The point where the popup attaches relative to the selection
  - **Popup Content**: The chat input and suggested actions within the popup
- **Chat Area**: The main container for the conversation
  - **Message List**: The scrollable area containing all messages
  - **Input Area**: The region where users can type their messages

#### Interaction States
- **Selection Active**: When text is currently selected and the popup is visible
- **Message Streaming**: When an AI message is actively being displayed with animation
- **Viewport Following**: The automatic scrolling behavior that keeps content in view

## Features

### Text Selection Menu
- Floating menu appears when text is selected
- Menu positions itself intelligently relative to the selected text
- Maintains text selection when interacting with menu controls
- Prevents accidental deselection when clicking menu items

### Chat Interface
- Real-time message streaming with animated text display
- Smart scrolling behavior that keeps content in view:
  - Messages stay visible as they animate in
  - Suggested responses are automatically scrolled into view
  - Maintains comfortable padding from bottom of viewport
- Suggested responses appear after AI messages
- Clean, modern styling with proper spacing and layout

### UX Improvements
- Smooth animations and transitions
- Intelligent viewport management
- Non-disruptive UI updates
- Preserves context during interactions

## Technical Details

The application uses:
- Elm for the core application logic
- JavaScript for DOM interactions and animations
- CSS for styling and animations
- ResizeObserver for tracking dynamic content changes
- MutationObserver for watching DOM updates

## Setup Instructions

### Prerequisites
- Node.js (v16 or later)
- npm or yarn
- Elm (v0.19.1 or later)
- OpenAI API key (for chat functionality)

### Environment Setup
Before running the application, you need to set up your environment variables:

1. Create a `.env` file in the root directory:
```bash
touch .env
```

2. Add your OpenAI API key to the `.env` file:
```
VITE_OPENAI_API=your-openai-api-key-here
```

To get an OpenAI API key:
1. Go to [OpenAI's API platform](https://platform.openai.com/account/api-keys)
2. Sign up or log in to your account
3. Create a new API key
4. Copy the key and paste it into your `.env` file

⚠️ **Important**: Never commit your `.env` file to version control. The `.env` file should be included in your `.gitignore`.

### Quick Start (For This Project)
1. Clone or download this repository
2. Install dependencies:
```bash
npm install
```

3. Set up your environment variables (see Environment Setup section above)

4. Start the development server:
```bash
npm run dev
```

5. Open your browser to `http://localhost:5173`

### Quick Start (Creating From Scratch)
1. Create a new project directory and initialize:
```bash
mkdir my-elm-chat
cd my-elm-chat
npm init -y
```

2. Install dependencies:
```bash
npm install --save-dev elm elm-live
```

3. Initialize Elm project:
```bash
elm init
```

4. Create the basic project structure:
```bash
mkdir src
mkdir public
```

5. Create a minimal `index.html` in the public directory:
```html
<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <title>Elm Chat</title>
    <link rel="stylesheet" href="styles.css">
</head>
<body>
    <div id="elm"></div>
    <script src="elm.js"></script>
    <script>
        var app = Elm.Main.init({
            node: document.getElementById('elm')
        });
    </script>
</body>
</html>
```

6. Add these scripts to your `package.json`:
```json
{
  "scripts": {
    "start": "elm-live src/Main.elm --open -- --output=public/elm.js",
    "build": "elm make src/Main.elm --output=public/elm.js --optimize"
  }
}
```

7. For Vercel deployment, create a `vercel.json` in the root:
```json
{
  "version": 2,
  "builds": [
    {
      "src": "package.json",
      "use": "@vercel/static-build",
      "config": {
        "distDir": "public"
      }
    }
  ],
  "routes": [
    {
      "src": "/(.*)",
      "dest": "/$1"
    }
  ]
}
```

8. Start development:
```bash
npm start
```

### Project Structure
```
my-elm-chat/
├── src/
│   ├── Main.elm
│   └── ... (other Elm modules)
├── public/
│   ├── index.html
│   ├── styles.css
│   └── elm.js (generated)
├── package.json
├── elm.json
└── vercel.json
```

### Deployment
1. Push your code to GitHub
2. Import the repository in Vercel
3. Deploy!

The project will automatically build and deploy when you push changes to your main branch.

## Key Components

### Selection Popup
- Tracks text selection events
- Positions itself relative to selected text
- Maintains selection state during interaction
- Custom click handling to prevent unwanted deselection

### Message Display
- Animated text streaming
- Real-time scroll adjustments
- Maintains visibility during animations
- Smart padding and spacing management

### Suggested Responses
- Dynamically positioned
- Automatically scrolled into view
- Maintains proper spacing from chat input
- Smooth transition animations