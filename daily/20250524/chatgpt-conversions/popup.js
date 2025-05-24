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