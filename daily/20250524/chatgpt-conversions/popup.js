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