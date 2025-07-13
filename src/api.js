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
- Only move to the next question after the user answers the current one
- Provide brief, helpful responses that acknowledge their choice and introduce the next question
- Format responses as JSON: {"content": "Your response", "suggestedResponses": ["option1", "option2", "option3"]}
- After all 5 questions are answered, provide a summary of their choices`
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

    // Add user message to history
    conversationHistory.push({
      role: "user",
      content: message
    });

    const completion = await openai.chat.completions.create({
      messages: conversationHistory,
      model: "gpt-3.5-turbo",
      response_format: { type: "json_object" }
    });

    // Add assistant's response to history
    const assistantMessage = completion.choices[0].message;
    conversationHistory.push(assistantMessage);

    try {
      // Parse the JSON response
      const parsedResponse = JSON.parse(assistantMessage.content);
      return {
        content: parsedResponse.content,
        suggestedResponses: parsedResponse.suggestedResponses || [],
        error: null
      };
    } catch (parseError) {
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