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

// Handle suggestions position measurement
if (app.ports && app.ports.requestSuggestionsPosition) {
  app.ports.requestSuggestionsPosition.subscribe(() => {
    // Find the messages container and suggested responses
    const messagesContainer = document.querySelector('.messages-container');
    const suggestedResponses = document.querySelector('.chat-input-container .suggested-responses');
    
    if (messagesContainer && suggestedResponses) {
      // Get positions of both elements
      const messagesRect = messagesContainer.getBoundingClientRect();
      const suggestionsRect = suggestedResponses.getBoundingClientRect();
      
      // Calculate the bottom of the messages container
      const messagesBottom = messagesRect.bottom;
      // Get the top of the suggested responses
      const suggestionsTop = suggestionsRect.top;
      
      // Calculate the gap between them
      const gap = suggestionsTop - messagesBottom;
      
      console.log('Messages bottom:', messagesBottom);
      console.log('Suggestions top:', suggestionsTop);
      console.log('Gap between them:', gap);
      
      // Send the gap back to Elm (negative gap means overlap, positive means space between)
      app.ports.suggestionsPositionReceived.send(Math.round(gap));
    } else if (suggestedResponses) {
      // Fallback: if only suggestions found, assume plenty of space
      console.log('Only suggestions found, assuming plenty of space');
      app.ports.suggestionsPositionReceived.send(200); // Large positive number = plenty of space
    } else {
      // If nothing found, assume we need to switch to inline
      console.log('No elements found, assuming collision');
      app.ports.suggestionsPositionReceived.send(-50); // Negative = collision
    }
  });
  }
  
  // Handle smooth scrolling to bottom of page
  if (app.ports && app.ports.scrollToBottom) {
    app.ports.scrollToBottom.subscribe(() => {
      // Smooth scroll to the bottom of the entire page
      window.scrollTo({
        top: document.body.scrollHeight,
        behavior: 'smooth'
      });
      console.log('Scrolling to bottom of page');
      
      // After scrolling, wait a bit and then check position for suggestions
      setTimeout(() => {
        console.log('Requesting position check after scroll');
        // Manually trigger the position measurement after scroll
        const messagesContainer = document.querySelector('.messages-container');
        const suggestedResponses = document.querySelector('.chat-input-container .suggested-responses');
        
        if (messagesContainer && suggestedResponses && app.ports && app.ports.suggestionsPositionReceived) {
          // Get positions of both elements
          const messagesRect = messagesContainer.getBoundingClientRect();
          const suggestionsRect = suggestedResponses.getBoundingClientRect();
          
          // Calculate the gap between them
          const gap = suggestionsRect.top - messagesRect.bottom;
          
          console.log('Post-scroll gap measurement:', gap);
          // Send the gap back to Elm
          app.ports.suggestionsPositionReceived.send(Math.round(gap));
        }
      }, 300); // Wait for scroll animation to complete
    });
  }
  
    