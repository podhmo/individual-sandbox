````json:manifest.json
{
  "manifest_version": 3,
  "name": "ChatGPT Conversation Exporter",
  "version": "1.0.3",
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
      width: 280px; /* Slightly wider for longer messages */
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
      box-sizing: border-box; /* Ensure padding doesn't make it wider */
    }
    button:hover {
      background-color: #e9e9e9;
    }
    button:disabled {
        background-color: #e0e0e0;
        color: #999;
        cursor: not-allowed;
    }
    #status {
      margin-top: 10px;
      font-size: 0.9em;
      min-height: 30px; /* Reserve space for multi-line messages */
      word-wrap: break-word;
      text-align: left;
      padding: 5px;
      border-radius: 3px;
    }
    .error {
      color: red;
      background-color: #ffebee;
      border: 1px solid #ffcdd2;
    }
    .success {
      color: green;
      background-color: #e8f5e9;
      border: 1px solid #c8e6c9;
    }
    .info {
      color: #0d47a1;
      background-color: #e3f2fd;
      border: 1px solid #bbdefb;
    }
  </style>
</head>
<body>
  <h4>ChatGPT Conversation Exporter</h4>
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

  let statusClearTimeout;

  function showStatus(message, type = 'info') { // type can be 'info', 'success', 'error'
    clearTimeout(statusClearTimeout); // Clear existing timeout
    statusDiv.textContent = message;
    statusDiv.className = type; // Assign class based on type

    // Auto-clear status after a delay, longer for errors
    const delay = type === 'error' ? 10000 : 5000;
    statusClearTimeout = setTimeout(() => {
        if (statusDiv.textContent === message) { // Clear only if it's the same message
            statusDiv.textContent = '';
            statusDiv.className = '';
        }
    }, delay);
  }

  async function handleDownload(action) {
    downloadJsonBtn.disabled = true;
    downloadTextBtn.disabled = true;
    const actionText = action === 'downloadJson' ? 'JSON' : 'Text';
    showStatus(`Processing ${actionText} download... Please wait.`, 'info');

    try {
      const response = await browser.runtime.sendMessage({ action });
      if (response && response.success) {
        showStatus(`${actionText} download initiated for ${response.filename || 'file'}.`, 'success');
      } else {
        showStatus(response.error || `Failed to download ${actionText}. Check console for details.`, 'error');
      }
    } catch (e) {
      console.error(`Error sending message for ${action} download:`, e);
      showStatus(`Client-side error: ${e.message || 'Unknown error occurred.'}. Check console.`, 'error');
    } finally {
      downloadJsonBtn.disabled = false;
      downloadTextBtn.disabled = false;
    }
  }

  downloadJsonBtn.addEventListener('click', () => handleDownload("downloadJson"));
  downloadTextBtn.addEventListener('click', () => handleDownload("downloadText"));

  // Initial check for active tab state
  browser.tabs.query({ active: true, currentWindow: true }).then(tabs => {
    if (tabs && tabs.length > 0 && tabs[0].url && /^https:\/\/chatgpt\.com\/c\/[a-f0-9-]+(-[a-f0-9-]+)*$/.test(tabs[0].url)) {
      showStatus("Ready to export. Ensure you've interacted with the chat if it was just loaded.", 'info');
      downloadJsonBtn.disabled = false;
      downloadTextBtn.disabled = false;
    } else {
      showStatus("Navigate to a ChatGPT conversation page (e.g., chatgpt.com/c/...). Buttons disabled.", 'error');
      downloadJsonBtn.disabled = true;
      downloadTextBtn.disabled = true;
    }
  }).catch(err => {
    console.error("Error querying tabs:", err);
    showStatus("Could not determine active tab state. Buttons disabled.", 'error');
    downloadJsonBtn.disabled = true;
    downloadTextBtn.disabled = true;
  });
});
````
````svg:icons/magnifying_glass.svg
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100 100" width="48" height="48">
  <title>Magnifying Glass Icon</title>
  <style>
    .icon-stroke { stroke: #333; }
    @media (prefers-color-scheme: dark) {
      .icon-stroke { stroke: #eee; }
    }
  </style>
  <circle cx="42" cy="42" r="30" class="icon-stroke" stroke-width="10" fill="none"/>
  <line x1="65" y1="65" x2="85" y2="85" class="icon-stroke" stroke-width="12" stroke-linecap="round"/>
</svg>
````
