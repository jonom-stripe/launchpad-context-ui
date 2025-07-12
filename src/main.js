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

 