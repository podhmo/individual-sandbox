document.addEventListener('DOMContentLoaded', () => {
  const downloadJsonBtn = document.getElementById('downloadJsonBtn');
  const downloadTextBtn = document.getElementById('downloadTextBtn');
  const statusDiv = document.getElementById('status');

  function showStatus(message, isError = false) {
    statusDiv.textContent = message;
    statusDiv.className = isError ? 'error' : 'success';
    // Clear status after a few seconds
    setTimeout(() => {
        if (statusDiv.textContent === message) { // Clear only if it's the same message
            statusDiv.textContent = '';
            statusDiv.className = '';
        }
    }, 5000);
  }

  downloadJsonBtn.addEventListener('click', async () => {
    statusDiv.textContent = 'Processing JSON download...';
    statusDiv.className = '';
    try {
      const response = await browser.runtime.sendMessage({ action: "downloadJson" });
      if (response && response.success) {
        showStatus('Raw JSON download initiated.');
      } else {
        showStatus(response.error || 'Failed to download JSON.', true);
      }
    } catch (e) {
      console.error("Error sending message for JSON download:", e);
      showStatus(`Error: ${e.message}`, true);
    }
  });

  downloadTextBtn.addEventListener('click', async () => {
    statusDiv.textContent = 'Processing Text download...';
    statusDiv.className = '';
    try {
      const response = await browser.runtime.sendMessage({ action: "downloadText" });
      if (response && response.success) {
        showStatus('Formatted text download initiated.');
      } else {
        showStatus(response.error || 'Failed to download formatted text.', true);
      }
    } catch (e) {
      console.error("Error sending message for Text download:", e);
      showStatus(`Error: ${e.message}`, true);
    }
  });
});