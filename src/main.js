import { Elm } from './Main.elm'
import './styles.css'
import { sendMessage } from './api.js'

// Initialize the Elm application with flags
const app = Elm.Main.init({
  node: document.getElementById('app'),
  flags: null
})

// Handle auto-growing textarea
function autoGrowTextarea() {
  const textareas = document.querySelectorAll('.chat-input');
  
  textareas.forEach(textarea => {
    // Set initial height
    textarea.style.height = 'auto';
    textarea.style.height = Math.max(96, Math.min(320, textarea.scrollHeight)) + 'px';
    
    // Add input event listener
    textarea.addEventListener('input', function() {
      this.style.height = 'auto';
      this.style.height = Math.max(96, Math.min(320, this.scrollHeight)) + 'px';
    });
  });
}

// Initialize auto-grow on page load and also observe for dynamically added textareas
document.addEventListener('DOMContentLoaded', autoGrowTextarea);

// Use MutationObserver to handle dynamically added textareas
const observer = new MutationObserver(() => {
  autoGrowTextarea();
});

observer.observe(document.body, {
  childList: true,
  subtree: true
});

// Handle chat messages
if (app.ports && app.ports.sendMessageToJs) {
  app.ports.sendMessageToJs.subscribe(async (message) => {
    try {
      const response = await sendMessage(message);
      
      if (response.error) {
        app.ports.messageReceived.send({
          content: "Sorry, there was an error processing your request.",
          suggestedResponses: []
        });
      } else {
        app.ports.messageReceived.send({
          content: response.content,
          suggestedResponses: response.suggestedResponses || []
        });
      }
    } catch (error) {
      app.ports.messageReceived.send({
        content: "Sorry, there was an error processing your request.",
        suggestedResponses: []
      });
    }
  });
}

// Handle client-side routing
if (app.ports && app.ports.pushUrl) {
  app.ports.pushUrl.subscribe(url => {
    window.history.pushState({}, '', url)
  })
}

// Handle suggestions position measurement with debouncing
let positionMeasurementTimeout;
if (app.ports && app.ports.requestSuggestionsPosition) {
  app.ports.requestSuggestionsPosition.subscribe(() => {
    // Clear any pending measurements to debounce
    if (positionMeasurementTimeout) {
      clearTimeout(positionMeasurementTimeout);
    }
    
    // Use requestAnimationFrame to ensure DOM is fully rendered
    requestAnimationFrame(() => {
      positionMeasurementTimeout = setTimeout(() => {
        // Find the messages container and input area
        const messagesContainer = document.querySelector('.messages-container');
        const inputContainer = document.querySelector('.chat-input-container');
        
        if (messagesContainer && inputContainer) {
          // Get the height of the conversation content
          const conversationHeight = messagesContainer.scrollHeight;
          
          // Get viewport height and input height
          const viewportHeight = window.innerHeight;
          const inputHeight = inputContainer.offsetHeight;
          
          // Calculate if conversation is too long for viewport (with 120px buffer)
          const availableHeight = viewportHeight - inputHeight - 120;
          const shouldUseInline = conversationHeight > availableHeight;
          
          console.log('Conversation height:', conversationHeight);
          console.log('Available height:', availableHeight);
          console.log('Should use inline:', shouldUseInline);
          
          // Send decision back to Elm (using negative/positive values to match existing logic)
          app.ports.suggestionsPositionReceived.send(shouldUseInline ? -10 : 100);
        } else {
          console.log('Elements not found, defaulting to input area');
          app.ports.suggestionsPositionReceived.send(100); // Default to input area
        }
      }, 16); // Small delay to ensure DOM stability
    });
  });
}
  
  // Handle smooth scrolling to bottom of page
  if (app.ports && app.ports.scrollToBottom) {
    app.ports.scrollToBottom.subscribe(() => {
      console.log('📜 ScrollToBottom triggered!');
      
      // Use requestAnimationFrame to ensure DOM is ready
      requestAnimationFrame(() => {
        // Calculate the full document height including any new content
        const documentHeight = Math.max(
          document.body.scrollHeight,
          document.body.offsetHeight,
          document.documentElement.clientHeight,
          document.documentElement.scrollHeight,
          document.documentElement.offsetHeight
        );
        
        // Smooth scroll to the bottom with a small buffer
        window.scrollTo({
          top: documentHeight,
          behavior: 'smooth'
        });
        
        console.log('📜 Scrolling to bottom - Document height:', documentHeight);
        
        // Also ensure chat messages are visible by scrolling the messages container
        const messagesContainer = document.querySelector('.messages-container');
        if (messagesContainer) {
          messagesContainer.scrollTop = messagesContainer.scrollHeight;
          console.log('📜 Also scrolled messages container');
        }
      });
    });
  }
  
    