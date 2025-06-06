type ChatMessage = {
  role: "system" | "user" | "assistant";
  content: string;
};

async function fetchChatCompletion(apiKey: string, messages: ChatMessage[]): Promise<string> {
  const url = "https://api.openai.com/v1/chat/completions";
  const headers = {
    "Content-Type": "application/json",
    "Authorization": `Bearer ${apiKey}`,
  };

  const body = {
    model: "gpt-4o-mini", // 使用するモデルを指定 ("gpt-4", "gpt-3.5-turbo", など)
    messages: messages,
    max_tokens: 100, // 必要に応じて変更
    temperature: 0.7, // 必要に応じて変更
  };

  try {
    const response = await fetch(url, {
      method: "POST",
      headers: headers,
      body: JSON.stringify(body),
    });

    if (!response.ok) {
      throw new Error(`Error: ${response.status} - ${response.statusText}`);
    }

    const data = await response.json();
    const completion = data.choices[0]?.message?.content;
    if (!completion) {
      throw new Error("No completion found in the response.");
    }

    return completion;
  } catch (error) {
    console.error("Failed to fetch chat completion:", error);
    throw error;
  }
}

// 使用例
(async () => {
  const apiKey = "your-api-key-here"; // OpenAIのAPIキーを設定
  
  const messages: ChatMessage[] = [
    { role: "system", content: "You are a helpful assistant." },
    { role: "user", content: "What is the capital of France?" },
  ];

  try {
    const completion = await fetchChatCompletion(apiKey, messages);
    console.log("AI Response:", completion);
  } catch (error) {
    console.error("Error:", error);
  }
})();
