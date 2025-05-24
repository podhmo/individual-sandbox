const CONVERSATION_API_URL_PATTERN = "*://*.chatgpt.com/backend-api/conversation/*";
const CHAT_PAGE_URL_PATTERN = "*://*.chatgpt.com/c/*";
// UUID regex: xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx
const UUID_REGEX = /^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$/;


// Helper to get active tab
async function getActiveTab() {
  const tabs = await browser.tabs.query({ active: true, currentWindow: true });
  if (tabs && tabs.length > 0) {
    // More specific match for chat page URL
    if (tabs[0].url && tabs[0].url.match(/^https:\/\/chatgpt\.com\/c\/[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}(\?.*)?$/)) {
        return tabs[0];
    }
  }
  return null;
}

// Listener for network requests to capture conversation API URLs and necessary headers
browser.webRequest.onSendHeaders.addListener(
  async (details) => {
    // Only process GET requests
    if (details.method !== "GET") {
      return;
    }

    if (details.tabId > 0 && details.url.includes("/backend-api/conversation/")) {
      try {
        const urlParts = details.url.split("/conversation/");
        if (urlParts.length < 2) return; 

        let conversationIdFromApiUrl = urlParts[1].split("?")[0]; // Remove query params

        // Validate if the conversationId part is a UUID
        if (!UUID_REGEX.test(conversationIdFromApiUrl)) {
          // console.log("Skipping non-UUID conversation ID in API URL:", conversationIdFromApiUrl, "from URL:", details.url);
          return; 
        }

        const tab = await browser.tabs.get(details.tabId);
        // Ensure the tab URL is a chat page (also typically uses UUID)
        if (tab && tab.url && tab.url.match(/^https:\/\/chatgpt\.com\/c\/[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}(\?.*)?$/)) {
          
          let capturedHeaders = {};
          const headersToCapture = [
            'authorization', 
            'oai-client-version',
            'user-agent',
            'accept',
            'accept-language',
            'referer', // Will be the chat page itself, potentially useful
            'oai-language',
            'oai-device-id'
          ];

          if (details.requestHeaders) {
            for (const header of details.requestHeaders) {
              if (headersToCapture.includes(header.name.toLowerCase())) {
                capturedHeaders[header.name] = header.value; // Preserve original casing for header name if needed, though HTTP/2 is lowercase
              }
            }
          }
          
          const storageKey = `conversationData_${conversationIdFromApiUrl}`;
          
          await browser.storage.local.set({ 
            [storageKey]: {
              apiUrl: details.url, 
              headers: capturedHeaders 
            }
          });
          // console.log(`Stored API data for conversation ${conversationIdFromApiUrl} (key: ${storageKey}): URL: ${details.url}, Headers:`, capturedHeaders);

          const chatPageUrlParts = tab.url.split("/c/");
          if (chatPageUrlParts.length > 1) {
            const chatPageConversationId = chatPageUrlParts[1].split("?")[0]; 
             if (chatPageConversationId && UUID_REGEX.test(chatPageConversationId)) {
                // Map the chat page's conversation ID (from URL) to the API's conversation ID (from API URL, which is usually the same)
                const tabUrlMapKey = `tabConvIdMap_${chatPageConversationId}`; 
                await browser.storage.local.set({ [tabUrlMapKey]: conversationIdFromApiUrl });
                // console.log(`Mapped Tab Conv ID ${chatPageConversationId} to API Conversation ID ${conversationIdFromApiUrl}`);
             }
          }
        }
      } catch (error) {
        if (!error.message || !error.message.startsWith("No tab with id:")) {
            console.error("Error in onSendHeaders listener:", error, "Details:", details);
        }
      }
    }
  },
  { urls: [CONVERSATION_API_URL_PATTERN] },
  ["requestHeaders"] 
);


// Listener for messages from the popup
browser.runtime.onMessage.addListener(async (request, sender, sendResponse) => {
  if (request.action === "downloadJson" || request.action === "downloadText") {
    const activeTab = await getActiveTab();
    if (!activeTab || !activeTab.url) {
      console.error("No active ChatGPT chat tab found or tab URL is missing.");
      sendResponse({ success: false, error: "Active ChatGPT chat tab not found." });
      return true;
    }

    let conversationIdFromTabUrl;
    const chatPageUrlParts = activeTab.url.split("/c/");
    if (chatPageUrlParts.length > 1) {
        conversationIdFromTabUrl = chatPageUrlParts[1].split("?")[0];
        if (!UUID_REGEX.test(conversationIdFromTabUrl)) {
            console.error("Invalid conversation ID format in active tab URL:", activeTab.url);
            sendResponse({ success: false, error: "Invalid conversation ID in tab URL."});
            return true;
        }
    } else {
        console.error("Could not extract conversation ID from tab URL:", activeTab.url);
        sendResponse({ success: false, error: "Could not determine conversation ID from tab URL." });
        return true;
    }
    
    // Use the conversation ID from the tab URL to find the mapped API conversation ID
    // (which should generally be the same if our logic is correct, but this mapping provides robustness)
    const tabUrlMapKey = `tabConvIdMap_${conversationIdFromTabUrl}`;
    const tabUrlMapData = await browser.storage.local.get(tabUrlMapKey);
    
    let apiConversationIdToFetch;
    if (tabUrlMapData && tabUrlMapData[tabUrlMapKey]) {
        apiConversationIdToFetch = tabUrlMapData[tabUrlMapKey];
        // console.log(`Found mapped API conversation ID: ${apiConversationIdToFetch} for tab conv ID ${conversationIdFromTabUrl}`);
    } else {
        // Fallback if no direct mapping found (should be rare if onSendHeaders worked)
        apiConversationIdToFetch = conversationIdFromTabUrl;
        // console.log(`Using conversation ID from tab URL as fallback for API fetch: ${apiConversationIdToFetch}`);
    }

    const storageKey = `conversationData_${apiConversationIdToFetch}`;
    const storedData = await browser.storage.local.get(storageKey);

    if (!storedData || !storedData[storageKey] || !storedData[storageKey].apiUrl) {
      console.error("API data (URL/headers) not found in storage for conversation ID:", apiConversationIdToFetch, "using key:", storageKey);
      sendResponse({ success: false, error: "Conversation data (API URL/headers) not found for this chat. Please reload the chat." });
      return true;
    }

    const { apiUrl, headers: capturedHeaders } = storedData[storageKey];

    try {
      // console.log(`Fetching from API URL: ${apiUrl} with headers:`, capturedHeaders);
      const response = await fetch(apiUrl, { 
        credentials: 'include', 
        headers: capturedHeaders 
      });

      if (!response.ok) {
        const errorBody = await response.text();
        console.error(`API request failed: ${response.status} ${response.statusText}. URL: ${apiUrl}. Body: ${errorBody}`);
        sendResponse({ success: false, error: `API Error ${response.status}: ${response.statusText}. Please ensure you are logged in and the conversation is accessible. (Details: ${errorBody.substring(0,100)})` });
        return true;
      }
      const jsonData = await response.json();
      
      const effectiveConversationId = jsonData.conversation_id || apiConversationIdToFetch || "unknown_conversation";
      const timestamp = new Date().toISOString().replace(/[:.]/g, '-');

      let blob;
      let filename;

      if (request.action === "downloadJson") {
        blob = new Blob([JSON.stringify(jsonData, null, 2)], { type: 'application/json' });
        filename = `chatgpt_conversation_raw_${effectiveConversationId}_${timestamp}.json`;
      } else { // downloadText
        const formattedText = formatConversationAsText(jsonData);
        blob = new Blob([formattedText], { type: 'text/plain;charset=utf-8' });
        filename = `chatgpt_conversation_formatted_${effectiveConversationId}_${timestamp}.txt`;
      }

      const downloadUrl = URL.createObjectURL(blob);
      await browser.downloads.download({
        url: downloadUrl,
        filename: filename,
        saveAs: true 
      });
      sendResponse({ success: true, filename });
    } catch (error) {
      console.error("Error during download process:", error);
      sendResponse({ success: false, error: `Download process error: ${error.message}` });
    }
    return true; 
  }
});

function formatConversationAsText(jsonData) {
  if (!jsonData || !jsonData.mapping || !jsonData.current_node) {
    return "Error: Invalid JSON data or missing 'mapping'/'current_node'. Check raw JSON.";
  }

  const { mapping, current_node, title, conversation_id } = jsonData;
  let orderedMessages = [];
  let currentNodeId = current_node;

  const processedNodeIds = new Set(); 

  while (currentNodeId && !processedNodeIds.has(currentNodeId)) {
    processedNodeIds.add(currentNodeId);
    const node = mapping[currentNodeId];
    if (!node) {
        console.warn("Node not found in mapping:", currentNodeId);
        break; 
    }

    if (node.message && node.message.author && node.message.content) {
      const msg = node.message;
      if (msg.author.role === "system" && 
          ( (msg.content.parts && msg.content.parts.join("").trim() === "") || msg.metadata?.is_visually_hidden_from_conversation) &&
          (!msg.content.text) 
          ) {
        // Skip
      } else if (msg.content.content_type === "text" || 
                 msg.content.content_type === "model_editable_context" ||
                 msg.content.content_type === "code" || 
                 msg.content.text 
                 ) { 
        
        let textContent = "";
        if (msg.content.parts && Array.isArray(msg.content.parts)) {
            textContent = msg.content.parts.map(part => {
                if (typeof part === 'string') return part;
                // Handle cases where 'part' might be an object containing text or other data
                if (typeof part === 'object' && part !== null) {
                    if (part.text) return part.text; // Common case for some structured content
                    // Add more specific handling if parts can be images, tools, etc.
                    // For now, just try to stringify if it's an unrecognized object, or ignore
                    // return JSON.stringify(part); // Or ignore: return ''; 
                }
                return '';
            }).join("");
        } else if (msg.content.text) { 
            textContent = msg.content.text;
        } else if (msg.content.content_type === "code" && msg.content.language && msg.content.text) {
            textContent = "```" + msg.content.language + "\n" + msg.content.text + "\n```";
        } else if (msg.content.content_type === "model_editable_context" && msg.content.model_set_context) {
            textContent = msg.content.model_set_context;
        }
        
        textContent = textContent.trim();

        if (textContent || msg.author.role === "user") {
             orderedMessages.push({
                role: msg.author.role,
                text: textContent,
                create_time: msg.create_time || node.create_time,
                id: msg.id
             });
        }
      }
    }
    currentNodeId = node.parent;
  }

  orderedMessages.reverse(); 
  
  const uniqueMessages = [];
  const seenIds = new Set();
  for (let i = orderedMessages.length - 1; i >= 0; i--) {
      const msg = orderedMessages[i];
      if (msg.id && !seenIds.has(msg.id)) {
          uniqueMessages.unshift(msg);
          seenIds.add(msg.id);
      } else if (!msg.id) { 
          uniqueMessages.unshift(msg);
      }
  }
  uniqueMessages.sort((a, b) => (a.create_time || 0) - (b.create_time || 0));

  let output = `Conversation Title: ${title || 'N/A'}\n`;
  output += `Conversation ID: ${conversation_id || 'N/A'}\n`;
  output += `Exported At: ${new Date().toLocaleString()}\n\n`;
  output += "----------------------------------------\n\n";

  uniqueMessages.forEach(msg => {
    if ((msg.role === "assistant" || msg.role === "system") && msg.text.trim() === "") return; 
    
    let roleDisplay = msg.role.charAt(0).toUpperCase() + msg.role.slice(1);
    // If msg.author.name is present, use it (e.g. for plugins, or future named roles)
    // For now, GPT roles are typically just 'user', 'assistant', 'system', 'tool'
    // The provided JSON example does not have msg.author.name, so this check is for future-proofing.
    // if (msg.author && msg.author.name) { 
    //     roleDisplay = msg.author.name;
    // }
    output += `${roleDisplay}:\n${msg.text}\n\n`;
  });

  return output.trim();
}