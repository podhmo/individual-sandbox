````json:manifest.json
{
  "manifest_version": 3,
  "name": "ChatGPT Conversation Exporter",
  "version": "1.0.2",
  "description": "Exports ChatGPT conversation history as raw JSON or formatted text, including necessary headers.",
  "permissions": [
    "tabs",
    "storage",
    "webRequest",
    "downloads",
    "scripting"
  ],
  "host_permissions": [
    "*://*.chatgpt.com/*"
  ],
  "background": {
    "scripts": ["background.js"]
  },
  "action": {
    "default_popup": "popup.html",
    "default_icon": {
      "16": "icons/magnifying_glass.svg",
      "48": "icons/magnifying_glass.svg",
      "128": "icons/magnifying_glass.svg"
    }
  },
  "icons": {
    "16": "icons/magnifying_glass.svg",
    "48": "icons/magnifying_glass.svg",
    "128": "icons/magnifying_glass.svg"
  }
}
````
````javascript:background.js
const CONVERSATION_API_URL_PATTERN = "*://*.chatgpt.com/backend-api/conversation/*";
const CHAT_PAGE_URL_PATTERN = "*://*.chatgpt.com/c/*";

// Helper to get active tab
async function getActiveTab() {
  const tabs = await browser.tabs.query({ active: true, currentWindow: true });
  if (tabs && tabs.length > 0) {
    if (tabs[0].url && tabs[0].url.match(/^https:\/\/chatgpt\.com\/c\/[a-zA-Z0-9-]+$/)) { // More specific match
        return tabs[0];
    }
  }
  return null;
}

// Listener for network requests to capture conversation API URLs and necessary headers
browser.webRequest.onSendHeaders.addListener(
  async (details) => {
    if (details.tabId > 0 && details.url.includes("/backend-api/conversation/")) {
      try {
        const tab = await browser.tabs.get(details.tabId);
        // Ensure the tab URL is a chat page
        if (tab && tab.url && tab.url.match(/^https:\/\/chatgpt\.com\/c\/[a-zA-Z0-9-]+$/)) {
          const conversationIdFromApiUrl = details.url.split("/conversation/")[1].split("?")[0];
          
          let capturedHeaders = {};
          if (details.requestHeaders) {
            for (const header of details.requestHeaders) {
              if (header.name.toLowerCase() === 'authorization' || header.name.toLowerCase() === 'oai-client-version') {
                capturedHeaders[header.name] = header.value;
              }
            }
          }

          // If we couldn't get a direct conversation ID from the tab URL (e.g. it's not loaded yet, or URL structure changes)
          // we rely on the conversationId from the API URL for storage key.
          // A more robust approach might involve content scripts to read the conversation ID from the page DOM if available.
          const storageKey = `conversationData_${conversationIdFromApiUrl}`;
          
          await browser.storage.local.set({ 
            [storageKey]: {
              apiUrl: details.url, // Store the full API URL including any query params if needed
              headers: capturedHeaders 
            }
          });
          console.log(`Stored API data for conversation ${conversationIdFromApiUrl} (key: ${storageKey}): URL: ${details.url}, Headers:`, capturedHeaders);

          // Also store a mapping from the tab's chat page URL to the conversation ID from API URL
          // This helps find the correct API URL when the popup is opened on a specific chat page.
          const chatPageUrlParts = tab.url.split("/c/");
          if (chatPageUrlParts.length > 1) {
            const chatPageConversationId = chatPageUrlParts[1].split("?")[0]; // Main part of conv ID from tab
             if (chatPageConversationId) {
                const tabUrlToConvIdKey = `tabUrlMap_${tab.url}`; // Map full tab URL to the API's conversation ID
                await browser.storage.local.set({ [tabUrlToConvIdKey]: conversationIdFromApiUrl });
                console.log(`Mapped Tab URL ${tab.url} to API Conversation ID ${conversationIdFromApiUrl}`);
             }
          }
        }
      } catch (error) {
        // Suppress "Error: No tab with id: XXX" if tab was closed quickly
        if (!error.message || !error.message.startsWith("No tab with id:")) {
            console.error("Error in onSendHeaders listener:", error, "Details:", details);
        }
      }
    }
  },
  { urls: [CONVERSATION_API_URL_PATTERN] },
  ["requestHeaders"] // We need this to access requestHeaders
);


// Listener for messages from the popup
browser.runtime.onMessage.addListener(async (request, sender, sendResponse) => {
  if (request.action === "downloadJson" || request.action === "downloadText") {
    const activeTab = await getActiveTab();
    if (!activeTab || !activeTab.url) {
      console.error("No active ChatGPT chat tab found or tab URL is missing.");
      sendResponse({ success: false, error: "Active ChatGPT chat tab not found." });
      return true; // Keep message channel open for async response
    }

    let conversationIdToFetch;
    // First, try to get the API's conversation ID mapped from the tab's full URL
    const tabUrlMapKey = `tabUrlMap_${activeTab.url}`;
    const tabUrlMapData = await browser.storage.local.get(tabUrlMapKey);

    if (tabUrlMapData && tabUrlMapData[tabUrlMapKey]) {
        conversationIdToFetch = tabUrlMapData[tabUrlMapKey];
        console.log(`Found mapped API conversation ID: ${conversationIdToFetch} for tab ${activeTab.url}`);
    } else {
        // Fallback: try to extract conversation ID from the tab URL directly
        // This might not always be the API conversation ID, but it's a best guess
        const chatPageUrlParts = activeTab.url.split("/c/");
        if (chatPageUrlParts.length > 1) {
            conversationIdToFetch = chatPageUrlParts[1].split("?")[0];
            console.log(`Using conversation ID from tab URL as fallback: ${conversationIdToFetch}`);
        } else {
            console.error("Could not determine conversation ID from tab URL:", activeTab.url);
            sendResponse({ success: false, error: "Could not determine conversation ID for this chat." });
            return true;
        }
    }
    
    const storageKey = `conversationData_${conversationIdToFetch}`;
    const storedData = await browser.storage.local.get(storageKey);

    if (!storedData || !storedData[storageKey] || !storedData[storageKey].apiUrl) {
      console.error("API data (URL/headers) not found in storage for conversation ID:", conversationIdToFetch, "using key:", storageKey);
      sendResponse({ success: false, error: "Conversation data (API URL/headers) not found for this chat." });
      return true;
    }

    const { apiUrl, headers: capturedHeaders } = storedData[storageKey];

    try {
      console.log(`Fetching from API URL: ${apiUrl} with headers:`, capturedHeaders);
      const response = await fetch(apiUrl, { 
        credentials: 'include', // Important for session cookies, if any
        headers: capturedHeaders   // Add the captured headers
      });

      if (!response.ok) {
        const errorBody = await response.text();
        throw new Error(`API request failed: ${response.status} ${response.statusText}. Body: ${errorBody.substring(0, 200)}`);
      }
      const jsonData = await response.json();
      
      // Use conversation_id from JSON if available, otherwise from the fetched ID
      const effectiveConversationId = jsonData.conversation_id || conversationIdToFetch || "unknown_conversation";
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
        saveAs: true // Prompts user for save location
      });
      // Consider revoking URL.createObjectURL(blob) after download, e.g., in browser.downloads.onChanged event.
      sendResponse({ success: true, filename });
    } catch (error) {
      console.error("Error during download process:", error);
      sendResponse({ success: false, error: error.message });
    }
    return true; // Indicates that the response will be sent asynchronously
  }
});

function formatConversationAsText(jsonData) {
  if (!jsonData || !jsonData.mapping || !jsonData.current_node) {
    return "Error: Invalid JSON data or missing 'mapping'/'current_node'. Check raw JSON.";
  }

  const { mapping, current_node, title, conversation_id } = jsonData;
  let orderedMessages = [];
  let currentNodeId = current_node;

  const processedNodeIds = new Set(); // To avoid infinite loops in malformed data

  while (currentNodeId && !processedNodeIds.has(currentNodeId)) {
    processedNodeIds.add(currentNodeId);
    const node = mapping[currentNodeId];
    if (!node) {
        console.warn("Node not found in mapping:", currentNodeId);
        break; 
    }

    if (node.message && node.message.author && node.message.content) {
      const msg = node.message;
      // Skip system messages with empty content or specific hidden messages (unless content is explicitly provided)
      if (msg.author.role === "system" && 
          ( (msg.content.parts && msg.content.parts.join("").trim() === "") || msg.metadata?.is_visually_hidden_from_conversation) &&
          (!msg.content.text) // Also check for direct text content in system messages
          ) {
        // Skip
      } else if (msg.content.content_type === "text" || 
                 msg.content.content_type === "model_editable_context" ||
                 msg.content.content_type === "code" || // Handle code blocks explicitly
                 msg.content.text // Some messages might just have a 'text' field
                 ) { 
        
        let textContent = "";
        if (msg.content.parts && Array.isArray(msg.content.parts)) {
            textContent = msg.content.parts.map(part => {
                if (typeof part === 'string') return part;
                if (typeof part === 'object' && part !== null && part.text) return part.text; // For complex part structures
                return '';
            }).join("");
        } else if (msg.content.text) { // Fallback for simple text content
            textContent = msg.content.text;
        } else if (msg.content.content_type === "code" && msg.content.language && msg.content.text) {
            textContent = "```" + msg.content.language + "\n" + msg.content.text + "\n```";
        } else if (msg.content.content_type === "model_editable_context" && msg.content.model_set_context) {
            textContent = msg.content.model_set_context;
        }
        
        textContent = textContent.trim();

        // Only add if there's actual content or it's a user message (even if empty)
        if (textContent || msg.author.role === "user") {
             orderedMessages.push({
                role: msg.author.role,
                text: textContent,
                create_time: msg.create_time || node.create_time, // Fallback to node create_time
                id: msg.id
             });
        }
      }
    }
    currentNodeId = node.parent;
  }

  orderedMessages.reverse(); // To get chronological order
  
  // Deduplicate based on message ID if present, preferring later entries (though reversal should handle it)
  const uniqueMessages = [];
  const seenIds = new Set();
  for (let i = orderedMessages.length - 1; i >= 0; i--) {
      const msg = orderedMessages[i];
      if (msg.id && !seenIds.has(msg.id)) {
          uniqueMessages.unshift(msg);
          seenIds.add(msg.id);
      } else if (!msg.id) { // If no ID, assume it's unique (or handle differently if needed)
          uniqueMessages.unshift(msg);
      }
  }
  // Optional: Sort by create_time as a final step if order is critical and IDs aren't fully reliable
  uniqueMessages.sort((a, b) => (a.create_time || 0) - (b.create_time || 0));


  let output = `Conversation Title: ${title || 'N/A'}\n`;
  output += `Conversation ID: ${conversation_id || 'N/A'}\n`;
  output += `Exported At: ${new Date().toLocaleString()}\n\n`;
  output += "----------------------------------------\n\n";


  uniqueMessages.forEach(msg => {
    // Skip genuinely empty system/assistant messages that might be structural artifacts
    if ((msg.role === "assistant" || msg.role === "system") && msg.text.trim() === "") return; 
    
    let roleDisplay = msg.role.charAt(0).toUpperCase() + msg.role.slice(1);
    if (msg.author && msg.author.name) { // If author name is available
        roleDisplay = msg.author.name;
    }
    output += `${roleDisplay}:\n${msg.text}\n\n`;
  });

  return output.trim();
}
````
````html:popup.html
<!DOCTYPE html>
<html>
<head>
  <meta charset="UTF-8">
  <title>ChatGPT Exporter</title>
  <style>
    body {
      font-family: sans-serif;
      width: 250px;
      padding: 10px;
      text-align: center;
    }
    button {
      display: block;
      width: 100%;
      padding: 10px;
      margin-bottom: 10px;
      border: 1px solid #ccc;
      background-color: #f9f9f9;
      cursor: pointer;
      border-radius: 4px;
    }
    button:hover {
      background-color: #e9e9e9;
    }
    button:disabled {
        background-color: #e0e0e0;
        cursor: not-allowed;
    }
    #status {
      margin-top: 10px;
      font-size: 0.9em;
      min-height: 20px; /* Reserve space for messages */
      word-wrap: break-word;
    }
    .error {
      color: red;
    }
    .success {
      color: green;
    }
  </style>
</head>
<body>
  <h3>Export Conversation</h3>
  <button id="downloadJsonBtn">Download Raw JSON</button>
  <button id="downloadTextBtn">Download Formatted Text</button>
  <div id="status"></div>
  <script src="popup.js"></script>
</body>
</html>
````
````javascript:popup.js
document.addEventListener('DOMContentLoaded', () => {
  const downloadJsonBtn = document.getElementById('downloadJsonBtn');
  const downloadTextBtn = document.getElementById('downloadTextBtn');
  const statusDiv = document.getElementById('status');

  function showStatus(message, isError = false) {
    statusDiv.textContent = message;
    statusDiv.className = isError ? 'error' : 'success';
    // Clear status after a few seconds unless it's a persistent error
    if (!isError || message.includes("initiated")) { // Keep error messages longer if they aren't just "initiated"
        setTimeout(() => {
            if (statusDiv.textContent === message) { // Clear only if it's the same message
                statusDiv.textContent = '';
                statusDiv.className = '';
            }
        }, isError ? 8000 : 5000); // Longer display for errors
    }
  }

  async function handleDownload(action) {
    downloadJsonBtn.disabled = true;
    downloadTextBtn.disabled = true;
    statusDiv.textContent = `Processing ${action === 'downloadJson' ? 'JSON' : 'Text'} download...`;
    statusDiv.className = '';

    try {
      const response = await browser.runtime.sendMessage({ action });
      if (response && response.success) {
        showStatus(`${action === 'downloadJson' ? 'Raw JSON' : 'Formatted text'} download initiated for ${response.filename || 'file'}.`);
      } else {
        showStatus(response.error || `Failed to download ${action === 'downloadJson' ? 'JSON' : 'text'}.`, true);
      }
    } catch (e) {
      console.error(`Error sending message for ${action} download:`, e);
      showStatus(`Error: ${e.message || 'Unknown error occurred.'}`, true);
    } finally {
      downloadJsonBtn.disabled = false;
      downloadTextBtn.disabled = false;
    }
  }

  downloadJsonBtn.addEventListener('click', () => handleDownload("downloadJson"));
  downloadTextBtn.addEventListener('click', () => handleDownload("downloadText"));

  // Check if on a valid ChatGPT page and update UI (optional, more complex to do reliably from popup alone)
  browser.tabs.query({ active: true, currentWindow: true }).then(tabs => {
    if (tabs && tabs.length > 0 && tabs[0].url && tabs[0].url.startsWith("https://chatgpt.com/c/")) {
      // Potentially enable buttons or show a ready message
    } else {
      showStatus("Please navigate to a ChatGPT conversation page (chatgpt.com/c/...).", true);
      downloadJsonBtn.disabled = true;
      downloadTextBtn.disabled = true;
    }
  }).catch(err => {
    console.error("Error querying tabs:", err);
    showStatus("Could not determine active tab.", true);
      downloadJsonBtn.disabled = true;
      downloadTextBtn.disabled = true;
  });

});
````
````svg:icons/magnifying_glass.svg
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100 100" width="48" height="48">
  <title>Magnifying Glass Icon</title>
  <style>
    @media (prefers-color-scheme: dark) {
      .icon-stroke { stroke: #eee; }
    }
    @media (prefers-color-scheme: light) {
      .icon-stroke { stroke: #333; }
    }
  </style>
  <circle cx="42" cy="42" r="30" class="icon-stroke" stroke-width="10" fill="none"/>
  <line x1="65" y1="65" x2="85" y2="85" class="icon-stroke" stroke-width="12" stroke-linecap="round"/>
</svg>
````
