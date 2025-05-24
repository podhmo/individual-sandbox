document.addEventListener('DOMContentLoaded', () => {
  const downloadJsonBtn = document.getElementById('downloadJsonBtn');
  const downloadTextBtn = document.getElementById('downloadTextBtn');
  const statusDiv = document.getElementById('status');
  // UUID regex for chat page URL check
  const CHAT_PAGE_UUID_REGEX = /^https:\/\/chatgpt\.com\/c\/[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}(\?.*)?$/;

  function showStatus(message, isError = false) {
    statusDiv.textContent = message;
    statusDiv.className = isError ? 'error' : 'success';
    
    // Clear status after a delay
    const delay = isError ? 8000 : 5000; // Longer display for errors
    setTimeout(() => {
        if (statusDiv.textContent === message) { 
            statusDiv.textContent = '';
            statusDiv.className = '';
        }
    }, delay);
  }

  async function handleDownload(action) {
    downloadJsonBtn.disabled = true;
    downloadTextBtn.disabled = true;
    statusDiv.textContent = `Processing ${action === 'downloadJson' ? 'JSON' : 'Text'} download... This may take a moment.`;
    statusDiv.className = '';

    try {
      // It's good practice to ensure browser.runtime is available
      if (!browser || !browser.runtime || !browser.runtime.sendMessage) {
          showStatus("Extension context error. Please reload the extension or browser.", true);
          return;
      }
      const response = await browser.runtime.sendMessage({ action });
      if (response && response.success) {
        showStatus(`${action === 'downloadJson' ? 'Raw JSON' : 'Formatted text'} download initiated for ${response.filename || 'file'}.`);
      } else {
        showStatus(response.error || `Failed to download ${action === 'downloadJson' ? 'JSON' : 'text'}.`, true);
      }
    } catch (e) {
      console.error(`Error sending message for ${action} download:`, e);
      showStatus(`Error: ${e.message || 'Unknown error occurred during request.'}`, true);
    } finally {
      // Re-enable buttons based on current tab state
      checkTabAndSetButtonState();
    }
  }

  downloadJsonBtn.addEventListener('click', () => handleDownload("downloadJson"));
  downloadTextBtn.addEventListener('click', () => handleDownload("downloadText"));

  function checkTabAndSetButtonState() {
     if (!browser || !browser.tabs || !browser.tabs.query) {
        showStatus("Cannot access browser tabs. Please reload extension.", true);
        downloadJsonBtn.disabled = true;
        downloadTextBtn.disabled = true;
        return;
    }
    browser.tabs.query({ active: true, currentWindow: true }).then(tabs => {
      if (tabs && tabs.length > 0 && tabs[0].url && CHAT_PAGE_UUID_REGEX.test(tabs[0].url)) {
        downloadJsonBtn.disabled = false;
        downloadTextBtn.disabled = false;
        // Optionally clear status if it was a "navigate to" message
        if (statusDiv.textContent.startsWith("Please navigate")) {
            statusDiv.textContent = '';
            statusDiv.className = '';
        }
      } else {
        showStatus("Please navigate to a valid ChatGPT conversation page (e.g., chatgpt.com/c/your-conversation-id).", true);
        downloadJsonBtn.disabled = true;
        downloadTextBtn.disabled = true;
      }
    }).catch(err => {
      console.error("Error querying tabs:", err);
      showStatus("Could not determine active tab state.", true);
      downloadJsonBtn.disabled = true;
      downloadTextBtn.disabled = true;
    });
  }

  // Initial check when popup opens
  checkTabAndSetButtonState();
});