const CONVERSATION_API_URL_PATTERN = "*://*.chatgpt.com/backend-api/conversation/*";
const CHAT_PAGE_URL_PATTERN = "*://*.chatgpt.com/c/*";

// Helper to get active tab
async function getActiveTab() {
  const tabs = await browser.tabs.query({ active: true, currentWindow: true });
  if (tabs && tabs.length > 0) {
    // Check if the active tab is a ChatGPT chat page
    if (tabs[0].url && tabs[0].url.startsWith("https://chatgpt.com/c/")) {
        return tabs[0];
    }
  }
  return null;
}

// Listener for network requests to capture conversation API URLs
browser.webRequest.onCompleted.addListener(
  async (details) => {
    if (details.tabId > 0 && details.url.includes("/backend-api/conversation/")) {
      try {
        const tab = await browser.tabs.get(details.tabId);
        if (tab && tab.url && tab.url.startsWith("https://chatgpt.com/c/")) {
          const conversationIdFromApiUrl = details.url.split("/conversation/")[1].split("?")[0];
          // Store mapping: chat page URL -> API URL
          // This is simplified; a more robust solution might store by conversation_id if available directly from tab or page content
          const storageKey = `conversationApiUrl_${conversationIdFromApiUrl}`;
          await browser.storage.local.set({ [storageKey]: details.url });
          console.log(`Stored API URL for ${tab.url} (key: ${storageKey}): ${details.url}`);

          // Also store a mapping from the tab's chat page URL to the conversation ID
          // This helps find the correct API URL when the popup is opened on a specific chat page.
          const chatPageConversationId = tab.url.split("/c/")[1].split("?")[0];
          if (chatPageConversationId) {
            const tabUrlToConvIdKey = `tabUrlToConvId_${tab.url}`;
            await browser.storage.local.set({ [tabUrlToConvIdKey]: chatPageConversationId });
            console.log(`Stored mapping: ${tab.url} -> ${chatPageConversationId}`);
          }

        }
      } catch (error) {
        console.error("Error in webRequest listener:", error);
      }
    }
  },
  { urls: [CONVERSATION_API_URL_PATTERN] }
);


// Listener for messages from the popup
browser.runtime.onMessage.addListener(async (request, sender, sendResponse) => {
  if (request.action === "downloadJson" || request.action === "downloadText") {
    const activeTab = await getActiveTab();
    if (!activeTab || !activeTab.url) {
      console.error("No active ChatGPT tab found or tab URL is missing.");
      sendResponse({ success: false, error: "No active ChatGPT tab found." });
      return;
    }

    // Try to get the conversation ID from the active tab's URL
    const chatPageUrlParts = activeTab.url.split("/c/");
    if (chatPageUrlParts.length < 2) {
        console.error("Could not extract conversation ID from tab URL:", activeTab.url);
        sendResponse({ success: false, error: "Could not determine conversation ID from tab URL." });
        return;
    }
    const conversationIdFromTab = chatPageUrlParts[1].split("?")[0];
    const storageKey = `conversationApiUrl_${conversationIdFromTab}`;


    const data = await browser.storage.local.get(storageKey);
    const apiUrl = data[storageKey];

    if (!apiUrl) {
      console.error("API URL not found in storage for tab:", activeTab.url, "using key:", storageKey);
      sendResponse({ success: false, error: "Conversation data API URL not found for this chat." });
      return;
    }

    try {
      const response = await fetch(apiUrl, { credentials: 'include' });
      if (!response.ok) {
        throw new Error(`API request failed: ${response.status} ${response.statusText}`);
      }
      const jsonData = await response.json();
      const conversationId = jsonData.conversation_id || conversationIdFromTab || "unknown_conversation";
      const timestamp = new Date().toISOString().replace(/[:.]/g, '-');

      let blob;
      let filename;

      if (request.action === "downloadJson") {
        blob = new Blob([JSON.stringify(jsonData, null, 2)], { type: 'application/json' });
        filename = `chatgpt_conversation_raw_${conversationId}_${timestamp}.json`;
      } else { // downloadText
        const formattedText = formatConversationAsText(jsonData);
        blob = new Blob([formattedText], { type: 'text/plain' });
        filename = `chatgpt_conversation_formatted_${conversationId}_${timestamp}.txt`;
      }

      const downloadUrl = URL.createObjectURL(blob);
      await browser.downloads.download({
        url: downloadUrl,
        filename: filename,
        saveAs: true
      });
      // URL.revokeObjectURL(downloadUrl); // Revoke after a delay or on completion
      sendResponse({ success: true });
    } catch (error) {
      console.error("Error during download process:", error);
      sendResponse({ success: false, error: error.message });
    }
    return true; // Indicates that the response will be sent asynchronously
  }
});

function formatConversationAsText(jsonData) {
  if (!jsonData || !jsonData.mapping || !jsonData.current_node) {
    return "Error: Invalid JSON data or missing 'mapping'/'current_node'.";
  }

  const { mapping, current_node } = jsonData;
  let orderedMessages = [];
  let currentNodeId = current_node;

  while (currentNodeId) {
    const node = mapping[currentNodeId];
    if (!node) break; // Should not happen in a valid structure

    if (node.message && node.message.author && node.message.content && node.message.content.parts) {
      // Skip system messages with empty content or specific hidden messages
      if (node.message.author.role === "system" && 
          (node.message.content.parts.join("").trim() === "" || node.message.metadata?.is_visually_hidden_from_conversation)) {
        // Move to parent and continue
      } else if (node.message.content.content_type === "text" || node.message.content.content_type === "model_editable_context") {
         // model_editable_context can sometimes be an empty container for a follow-up assistant message.
         // We'll include it if it has parts, otherwise it might be part of a multi-turn assistant response.
        const textContent = node.message.content.parts?.join("").trim();
        if (textContent || node.message.author.role !== "assistant" || node.message.content.content_type !== "model_editable_context") {
             orderedMessages.push({
                role: node.message.author.role,
                text: textContent || (node.message.content.model_set_context || ""), // Handle model_editable_context if parts is empty
                create_time: node.message.create_time || node.create_time // Fallback to node create_time
             });
        }
      }
    }
    currentNodeId = node.parent;
  }

  // Reverse to get chronological order and filter out any potential duplicates if create_time is used for sorting later
  orderedMessages.reverse();
  
  // Sort by create_time just in case the parent-child traversal wasn't strictly chronological (though it usually is for the main path)
  // Ensure create_time exists for robust sorting.
  orderedMessages.sort((a, b) => (a.create_time || 0) - (b.create_time || 0));


  let output = `Conversation Title: ${jsonData.title || 'N/A'}\n`;
  output += `Conversation ID: ${jsonData.conversation_id || 'N/A'}\n`;
  output += `Exported At: ${new Date().toLocaleString()}\n\n`;
  output += "----------------------------------------\n\n";


  orderedMessages.forEach(msg => {
    if (msg.text.trim() === "" && msg.role === "assistant") return; // Skip empty assistant messages that might be structural
    output += `${msg.role.charAt(0).toUpperCase() + msg.role.slice(1)}:\n${msg.text}\n\n`;
  });

  return output.trim();
}