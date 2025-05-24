const CONVERSATION_API_URL_PATTERN = "*://*.chatgpt.com/backend-api/conversation/*";
// const CHAT_PAGE_URL_PATTERN = "*://*.chatgpt.com/c/*"; // Not directly used for matching active tab anymore, but for context.

const REQUEST_HEADERS_TO_CAPTURE = [
  'authorization',
  'oai-client-version',
  'user-agent',
  'accept',
  'accept-language',
  'referer', // Will be captured from original request
  'oai-language',
  'oai-device-id'
];

// Helper to get active tab that looks like a ChatGPT chat page
async function getActiveChatGPTChatTab() {
  const tabs = await browser.tabs.query({ active: true, currentWindow: true });
  if (tabs && tabs.length > 0) {
    // More robust check for a chat page URL (e.g., https://chatgpt.com/c/conversation-uuid)
    if (tabs[0].url && /^https:\/\/chatgpt\.com\/c\/[a-f0-9-]+(-[a-f0-9-]+)*$/.test(tabs[0].url)) {
        return tabs[0];
    }
  }
  return null;
}

browser.webRequest.onSendHeaders.addListener(
  async (details) => {
    // Only process requests for the conversation API and from a tab
    if (details.tabId <= 0 || !details.url.includes("/backend-api/conversation/")) {
      return;
    }

    try {
      const tab = await browser.tabs.get(details.tabId);
      // Ensure the tab URL is a chat page (or at least chatgpt.com)
      if (tab && tab.url && tab.url.startsWith("https://chatgpt.com/")) {
        const conversationIdFromApiUrl = details.url.split("/conversation/")[1].split("?")[0];
        
        let capturedHeaders = {};
        if (details.requestHeaders) {
          for (const header of details.requestHeaders) {
            const lowerCaseHeaderName = header.name.toLowerCase();
            if (REQUEST_HEADERS_TO_CAPTURE.includes(lowerCaseHeaderName)) {
              capturedHeaders[header.name] = header.value; // Preserve original case for header name if needed, though HTTP/2 is case-insensitive
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

        // Map the tab's chat page URL to the conversation ID from API URL
        if (tab.url.includes("/c/")) {
            const chatPageUrlParts = tab.url.split("/c/");
            if (chatPageUrlParts.length > 1) {
                const chatPageConversationId = chatPageUrlParts[1].split("?")[0]; 
                if (chatPageConversationId) {
                    // Store a mapping: full tab URL -> API's conversation ID (the one in the API URL)
                    const tabUrlToApiConvIdKey = `tabUrlMap_${tab.url}`; 
                    await browser.storage.local.set({ [tabUrlToApiConvIdKey]: conversationIdFromApiUrl });
                    // console.log(`Mapped Tab URL ${tab.url} to API Conversation ID ${conversationIdFromApiUrl}`);
                }
            }
        }
      }
    } catch (error) {
      if (!error.message || !error.message.startsWith("No tab with id:")) {
          console.error("Error in onSendHeaders listener:", error, "Details:", details);
      }
    }
  },
  { urls: [CONVERSATION_API_URL_PATTERN] },
  ["requestHeaders"]
);


browser.runtime.onMessage.addListener(async (request, sender, sendResponse) => {
  if (request.action === "downloadJson" || request.action === "downloadText") {
    const activeTab = await getActiveChatGPTChatTab();
    if (!activeTab || !activeTab.url) {
      sendResponse({ success: false, error: "Active ChatGPT chat tab not found. Please open a chat." });
      return true;
    }

    let conversationIdToFetch;
    const tabUrlMapKey = `tabUrlMap_${activeTab.url}`;
    const tabUrlMapData = await browser.storage.local.get(tabUrlMapKey);

    if (tabUrlMapData && tabUrlMapData[tabUrlMapKey]) {
        conversationIdToFetch = tabUrlMapData[tabUrlMapKey];
    } else {
        // Fallback: try to extract conversation ID from the tab URL (might not be the API one)
        const chatPageUrlParts = activeTab.url.split("/c/");
        if (chatPageUrlParts.length > 1) {
            conversationIdToFetch = chatPageUrlParts[1].split("?")[0];
        } else {
            sendResponse({ success: false, error: "Could not determine conversation ID for this chat." });
            return true;
        }
    }
    
    const storageKey = `conversationData_${conversationIdToFetch}`;
    const storedDataResult = await browser.storage.local.get(storageKey);
    const storedData = storedDataResult ? storedDataResult[storageKey] : null;

    if (!storedData || !storedData.apiUrl || !storedData.headers) {
      sendResponse({ success: false, error: `Conversation data (API URL/headers) not found for ID ${conversationIdToFetch}. Please interact with the chat or reload the page to capture data.` });
      return true;
    }

    const { apiUrl, headers: capturedHeaders } = storedData;

    // Ensure 'Referer' is set to the current tab's URL if not captured or if it needs to be dynamic
    // However, using the captured 'Referer' is generally safer if the API expects the original one.
    // If capturedHeaders.Referer is not present, or if you decide it MUST be the current tab:
    if (!capturedHeaders['Referer'] && !capturedHeaders['referer']) { // check both cases
        capturedHeaders['Referer'] = activeTab.url; 
    }


    try {
      // console.log(`Fetching from API URL: ${apiUrl} with headers:`, JSON.stringify(capturedHeaders, null, 2));
      const response = await fetch(apiUrl, { 
        method: 'GET', // Explicitly GET
        credentials: 'include', 
        headers: capturedHeaders
      });

      if (!response.ok) {
        let errorBodyText = "Could not read error body.";
        try {
            errorBodyText = await response.text();
        } catch (e) { /* ignore */ }
        throw new Error(`API request failed: ${response.status} ${response.statusText}. Response: ${errorBodyText.substring(0, 500)}`);
      }
      const jsonData = await response.json();
      
      const effectiveConversationId = jsonData.conversation_id || conversationIdToFetch || "unknown_conversation";
      const timestamp = new Date().toISOString().replace(/[:.]/g, '-');

      let blob;
      let filename;

      if (request.action === "downloadJson") {
        blob = new Blob([JSON.stringify(jsonData, null, 2)], { type: 'application/json;charset=utf-8' });
        filename = `chatgpt_conversation_raw_${effectiveConversationId}_${timestamp}.json`;
      } else { // downloadText
        const formattedText = formatConversationAsText(jsonData);
        blob = new Blob([formattedText], { type: 'text/plain;charset=utf-8' });
        filename = `chatgpt_conversation_formatted_${effectiveConversationId}_${timestamp}.txt`;
      }

      const downloadUrl = URL.createObjectURL(blob);
      try {
        await browser.downloads.download({
          url: downloadUrl,
          filename: filename,
          saveAs: true
        });
        sendResponse({ success: true, filename });
      } catch (downloadError) {
          console.error("Download failed:", downloadError);
          sendResponse({ success: false, error: `Download initiation failed: ${downloadError.message}`});
      } finally {
          URL.revokeObjectURL(downloadUrl); // Revoke immediately after download is initiated
      }

    } catch (error) {
      console.error("Error during download process:", error);
      sendResponse({ success: false, error: error.message });
    }
    return true;
  }
});

function formatConversationAsText(jsonData) {
  if (!jsonData || typeof jsonData.mapping !== 'object' || !jsonData.current_node) {
    return "Error: Invalid JSON data structure. 'mapping' or 'current_node' is missing or invalid. Check raw JSON.";
  }

  const { mapping, current_node, title, conversation_id } = jsonData;
  let orderedMessages = [];
  let currentNodeId = current_node;
  const processedNodeIds = new Set(); 

  while (currentNodeId && mapping[currentNodeId] && !processedNodeIds.has(currentNodeId)) {
    processedNodeIds.add(currentNodeId);
    const node = mapping[currentNodeId];
    
    if (node.message && node.message.author && node.message.content) {
      const msg = node.message;
      let textContent = "";
      let skipMessage = false;

      if (msg.author.role === "system") {
        if ((!msg.content.parts || msg.content.parts.join("").trim() === "") && 
            (!msg.content.text || msg.content.text.trim() === "") &&
            (msg.metadata?.is_visually_hidden_from_conversation || msg.metadata?.gizmo_id)) {
          skipMessage = true; // Skip purely structural or hidden system messages
        }
      }
      
      if (!skipMessage) {
        if (msg.content.parts && Array.isArray(msg.content.parts)) {
            textContent = msg.content.parts.map(part => {
                if (typeof part === 'string') return part;
                // Handle more complex parts if necessary, e.g. DALL-E images, code interpreter results.
                // For now, focus on text.
                if (typeof part === 'object' && part !== null && part.content_type === 'text' && part.text) return part.text;
                if (typeof part === 'object' && part !== null && part.text) return part.text; // Generic object with text
                return '';
            }).join("");
        } else if (msg.content.text) {
            textContent = msg.content.text;
        } else if (msg.content.content_type === "code" && msg.content.language && typeof msg.content.text === 'string') {
            textContent = "```" + msg.content.language + "\n" + msg.content.text + "\n```";
        } else if (msg.content.content_type === "model_editable_context" && typeof msg.content.model_set_context === 'string') {
            textContent = msg.content.model_set_context;
        }
        
        textContent = textContent.trim();

        if (textContent || msg.author.role === "user" || (msg.author.role === "assistant" && node.children && node.children.length === 0) ) { // Include user messages even if empty, or last assistant message
             orderedMessages.push({
                role: msg.author.role,
                name: msg.author.name, // Capture author name if available (e.g., for plugins/GPTs)
                text: textContent,
                create_time: msg.create_time || node.create_time,
                id: msg.id || currentNodeId // Use message ID or node ID as fallback
             });
        }
      }
    }
    currentNodeId = node.parent; // Move to the parent node
  }

  orderedMessages.reverse(); 
  
  const uniqueMessages = [];
  const seenIds = new Set();
  for (const msg of orderedMessages) { // Iterate forward after reversal
      if (msg.id && !seenIds.has(msg.id)) {
          uniqueMessages.push(msg);
          seenIds.add(msg.id);
      } else if (!msg.id && msg.text) { // If no ID, include if it has text
          uniqueMessages.push(msg);
      } else if (msg.role === 'user' && !msg.id) { // Include user messages without ID
          uniqueMessages.push(msg);
      }
  }
  // Optional: Sort by create_time as a final step
  uniqueMessages.sort((a, b) => (a.create_time || 0) - (b.create_time || 0));

  let output = `Conversation Title: ${title || 'N/A'}\n`;
  output += `Conversation ID: ${conversation_id || 'N/A'}\n`;
  output += `Exported At: ${new Date().toLocaleString()}\n\n`;
  output += "----------------------------------------\n\n";

  uniqueMessages.forEach(msg => {
    if ((msg.role === "assistant" || msg.role === "system") && msg.text.trim() === "" && msg.id && !mapping[msg.id]?.children?.length) {
        // More aggressive skipping of empty assistant/system messages unless they are leaf nodes
        // This can be tuned based on observed JSON structure
        // return; 
    }
    
    let roleDisplay = msg.name || (msg.role.charAt(0).toUpperCase() + msg.role.slice(1));
    output += `${roleDisplay}:\n${msg.text || '(empty message)'}\n\n`;
  });

  return output.trim();
}