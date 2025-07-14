import OpenAI from 'openai';

const openai = new OpenAI({
  apiKey: import.meta.env.VITE_OPENAI_API,
  dangerouslyAllowBrowser: true // Note: In production, you should use a backend server
});

const INITIAL_MESSAGE = {
  content: "Thank you for providing that information about your business. To help set up your Stripe integration, I need to know your preferences.\n\nWhat is your business model: a SaaS platform or a Marketplace?",
  suggestedResponses: [
    "SaaS platform",
    "Marketplace", 
    "I'm not sure"
  ]
};

// Keep track of conversation history and flow
let conversationHistory = [
  {
    role: "system",
    content: `You are helping a user set up their Stripe integration by following a specific conversation flow. Guide them through these questions IN ORDER:

1. BUSINESS MODEL: "What is your business model: a SaaS platform or a Marketplace?"
   - Suggested responses: ["SaaS platform", "Marketplace", "I'm not sure"]

2. FEES: "How do you want to collect and pay for fees?"
   - Suggested responses: ["Seller pays fees", "You pay fees", "What are the benefits?"]

3. ONBOARDING: "How do you want your users to onboard to your platform?"
   - Suggested responses: ["With a Stripe-hosted onboarding flow", "With an embedded onboarding flow", "I want to build my own onboarding flow"]

4. CHECKOUT: "How will buyers pay your sellers: With Stripe-hosted Checkout, embedded components on your site, or with payment links?"
   - Suggested responses: ["Use Checkout", "Embed components into my site", "Use payment links"]

5. DASHBOARD: "How will sellers manage their account: with the Stripe Dashboard or embedded components on your site?"
   - Suggested responses: ["Stripe Dashboard", "Embedded components", "I'm not sure"]

RULES:
- Ask questions one at a time in the exact order above
- Use the EXACT question text provided
- Use the EXACT suggested responses provided  
- For decisive answers (like "SaaS platform", "Marketplace", "Seller pays fees", etc.): Provide a brief acknowledgment of their choice, then IMMEDIATELY ask the next question in the sequence with its exact suggested responses
- For informational requests (like "I'm not sure", "What are the benefits?", or any question): First provide a detailed explanation of the options, then re-ask the SAME question with the same suggested responses
- Only move to the next question after the user gives a decisive answer
- Format responses as JSON: {"content": "Your response", "suggestedResponses": ["option1", "option2", "option3"]}
- After all 5 questions are answered, provide a summary of their choices and offer: ["Edit my integration", "Walk me through the codebase"]

EXAMPLES:
- If user says "SaaS platform" to business model question: {"content": "Great choice! A SaaS platform is perfect for subscription-based services. How do you want to collect and pay for fees?", "suggestedResponses": ["Seller pays fees", "You pay fees", "What are the benefits?"]}
- If user says "I'm not sure" to business model question: {"content": "Let me explain the options. A SaaS platform... A Marketplace... What is your business model: a SaaS platform or a Marketplace?", "suggestedResponses": ["SaaS platform", "Marketplace", "I'm not sure"]}
- If user says "What are the benefits?" to fees question: {"content": "Here are the benefits of each fee structure... How do you want to collect and pay for fees?", "suggestedResponses": ["Seller pays fees", "You pay fees", "What are the benefits?"]}`
  }
];

export async function sendMessage(message) {
  try {
    // Handle initial empty message
    if (message === "") {
      return {
        ...INITIAL_MESSAGE,
        error: null
      };
    }

    // Handle contextual responses for navigation
    if (message.includes("I'd like to make changes to my")) {
      return handleSectionChangeRequest(message);
    }
    
    if (message.includes("Let's continue setting up")) {
      return handleContinueRequest();
    }

    // Handle completion flow responses  
    if (message.includes("Edit my integration")) {
      return handleEditIntegrationRequest();
    }
    
    if (message.includes("Walk me through the codebase")) {
      return handleCodebaseWalkthrough();
    }

    // Add user message to history
    conversationHistory.push({
      role: "user",
      content: message
    });

    console.log('Sending to OpenAI:', {
      message: message,
      conversationLength: conversationHistory.length,
      lastFewMessages: conversationHistory.slice(-3)
    });

    const completion = await openai.chat.completions.create({
      messages: conversationHistory,
      model: "gpt-3.5-turbo",
      response_format: { type: "json_object" }
    });

    // Add assistant's response to history
    const assistantMessage = completion.choices[0].message;
    console.log('Received from OpenAI:', assistantMessage.content);
    
    conversationHistory.push(assistantMessage);

    try {
      // Parse the JSON response
      const parsedResponse = JSON.parse(assistantMessage.content);
      console.log('Parsed response:', parsedResponse);
      return {
        content: parsedResponse.content,
        suggestedResponses: parsedResponse.suggestedResponses || [],
        error: null
      };
    } catch (parseError) {
      console.error('JSON parsing failed:', parseError, 'Raw content:', assistantMessage.content);
      // If JSON parsing fails, return the message as-is without suggestions
      return {
        content: assistantMessage.content,
        suggestedResponses: [],
        error: null
      };
    }
  } catch (error) {
    console.error('Error calling OpenAI:', error);
    return {
      content: null,
      suggestedResponses: [],
      error: error.message
    };
  }
}

// Handle requests to change a specific section setup
function handleSectionChangeRequest(message) {
  const sectionResponses = {
    "business model": {
      content: "Let's revisit your business model setup. What is your business model: a SaaS platform or a Marketplace?",
      suggestedResponses: ["SaaS platform", "Marketplace", "I'm not sure"]
    },
    "onboarding": {
      content: "Let's revisit your onboarding setup. How do you want your users to onboard to your platform?",
      suggestedResponses: ["With a Stripe-hosted onboarding flow", "With an embedded onboarding flow", "I want to build my own onboarding flow"]
    },
    "checkout": {
      content: "Let's revisit your checkout setup. How will buyers pay your sellers: With Stripe-hosted Checkout, embedded components on your site, or with payment links?",
      suggestedResponses: ["Use Checkout", "Embed components into my site", "Use payment links"]
    },
    "dashboard": {
      content: "Let's revisit your dashboard setup. How will sellers manage their account: with the Stripe Dashboard or embedded components on your site?",
      suggestedResponses: ["Stripe Dashboard", "Embedded components", "I'm not sure"]
    }
  };

  // Extract section name from message
  for (const [section, response] of Object.entries(sectionResponses)) {
    if (message.toLowerCase().includes(section)) {
      // Reset conversation to that specific question
      conversationHistory = [
        conversationHistory[0], // Keep system message
        {
          role: "user",
          content: `I want to build an online platform`
        }
      ];
      
      return {
        ...response,
        error: null
      };
    }
  }

  // Fallback if section not recognized
  return {
    content: "I understand you'd like to make changes. Could you clarify which section you'd like to modify?",
    suggestedResponses: ["Business model", "Onboarding", "Checkout", "Dashboard"],
    error: null
  };
}

// Handle requests to continue where left off
function handleContinueRequest() {
  // Determine where the user was in the conversation based on history
  const userMessages = conversationHistory.filter(msg => msg.role === 'user').length;
  
  // Continue with the next appropriate question based on progress
  if (userMessages <= 2) {
    return {
      content: "Great! Let's continue. How do you want to collect and pay for fees?",
      suggestedResponses: ["Seller pays fees", "You pay fees", "What are the benefits?"]
    };
  } else if (userMessages <= 3) {
    return {
      content: "Perfect! Let's move on. How do you want your users to onboard to your platform?",
      suggestedResponses: ["With a Stripe-hosted onboarding flow", "With an embedded onboarding flow", "I want to build my own onboarding flow"]
    };
  } else if (userMessages <= 4) {
    return {
      content: "Excellent! Now let's talk about payments. How will buyers pay your sellers: With Stripe-hosted Checkout, embedded components on your site, or with payment links?",
      suggestedResponses: ["Use Checkout", "Embed components into my site", "Use payment links"]
    };
  } else if (userMessages <= 5) {
    return {
      content: "Almost done! Finally, how will sellers manage their account: with the Stripe Dashboard or embedded components on your site?",
      suggestedResponses: ["Stripe Dashboard", "Embedded components", "I'm not sure"]
    };
  } else {
    return {
      content: "It looks like we've covered all the main setup questions! Would you like to review any specific section or do you have other questions about your Stripe integration?",
      suggestedResponses: ["Edit my integration", "Walk me through the codebase"],
      error: null
    };
  }
} 

// Handle "Edit my integration" request
function handleEditIntegrationRequest() {
  return {
    content: "I'd be happy to help you edit your integration setup. What would you like to modify?",
    suggestedResponses: [
      "Business model", 
      "Onboarding", 
      "Checkout", 
      "Dashboard"
    ],
    error: null
  };
}

// Handle "Walk me through the codebase" request  
function handleCodebaseWalkthrough() {
  return {
    content: "Let me walk you through the codebase! I'm now showing you the App.jsx file, which is the main component that brings everything together. This React component imports Stripe's loadStripe function and sets up the payment flow. You can see how we initialize Stripe with your publishable key, create the checkout session, and handle the payment elements. The component structure shows a clean separation between the payment form and the backend integration.",
    suggestedResponses: [],
    error: null
  };
} 