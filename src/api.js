import OpenAI from 'openai';

const openai = new OpenAI({
  apiKey: import.meta.env.VITE_OPENAI_API,
  dangerouslyAllowBrowser: true // Note: In production, you should use a backend server
});

const INITIAL_MESSAGE = {
  content: "Welcome to your custom Stripe integration plan. You can see an overview of everything we discussed on the right. Have a look around and let me know if you have questions.",
  suggestedResponses: [
    "Tell me more about my integration plan",
    "What are the key features?",
    "How do I get started?"
  ]
};

// Keep track of conversation history
let conversationHistory = [
  {
    role: "system",
    content: `You are a helpful AI assistant, focsued only on helping building a Stripe integration. Provide clear, concise responses.
    Stay focused on things a user can do before setting up a Stripe account. Assume the user is building an integration for a SaaS product using hosted Checkout. For each response, also suggest up to 3 relevant follow-up questions or responses that the user might want to ask.
    Format your response as a JSON object with 'content' and 'suggestedResponses' fields.
    Example: {"content": "Your response here", "suggestedResponses": ["Follow up 1", "Follow up 2", "Follow up 3"]}`
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