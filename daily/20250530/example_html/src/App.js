import { h } from 'preact';
import { useState, useEffect, useCallback } from 'preact/hooks';
import htm from 'htm';

import { parseMarkdown } from './utils/markdownParser.js';
import Header from './components/Header.js';
import CategoryView from './views/CategoryView.js';
import PromptListView from './views/PromptListView.js';
import PromptDetailView from './views/PromptDetailView.js';

const html = htm.bind(h);

const App = () => {
  const [promptsData, setPromptsData] = useState([]);
  const [currentPath, setCurrentPath] = useState(location.pathname);
  const [error, setError] = useState(null);
  const [isLoading, setIsLoading] = useState(true);
  const [toastMessage, setToastMessage] = useState('');

  const showToast = (message) => {
    setToastMessage(message);
    setTimeout(() => setToastMessage(''), 3000);
  };

  const loadAndParseMarkdown = useCallback(async (markdownContent, sourceName = 'Markdown') => {
    setIsLoading(true);
    setError(null);
    try {
      if (!markdownContent) {
        throw new Error("Markdown content is empty or not loaded.");
      }
      const parsedData = parseMarkdown(markdownContent);
      if (!parsedData || parsedData.length === 0) {
        throw new Error(`No prompts found in ${sourceName}. Please check the Markdown format.`);
      }
      setPromptsData(parsedData);
    } catch (err) {
      console.error('Error parsing markdown:', err);
      setError(`Error loading/parsing ${sourceName}: ${err.message}`);
      setPromptsData([]); // Clear data on error
    } finally {
      setIsLoading(false);
    }
  }, []);

  const loadDefaultMarkdown = useCallback(async () => {
    try {
      const response = await fetch('Template.md');
      if (!response.ok) {
        throw new Error(`Failed to load Template.md: ${response.statusText}`);
      }
      const markdownText = await response.text();
      loadAndParseMarkdown(markdownText, 'Template.md');
    } catch (err) {
      console.error('Error loading default markdown:', err);
      setError(`Failed to load default Template.md: ${err.message}. You can try selecting a local file.`);
      setPromptsData([]);
      setIsLoading(false);
    }
  }, [loadAndParseMarkdown]);

  useEffect(() => {
    loadDefaultMarkdown();
  }, [loadDefaultMarkdown]);

  useEffect(() => {
    const handlePopState = () => {
      setCurrentPath(location.pathname);
    };
    window.addEventListener('popstate', handlePopState);
    return () => window.removeEventListener('popstate', handlePopState);
  }, []);

  const navigate = (path) => {
    if (location.pathname !== path) {
      history.pushState(null, '', path);
    }
    setCurrentPath(path);
  };

  const handleFileChange = async (event) => {
    const file = event.target.files[0];
    if (file) {
      const reader = new FileReader();
      reader.onload = async (e) => {
        await loadAndParseMarkdown(e.target.result, file.name);
        navigate('/'); // Go to home after loading new file
      };
      reader.onerror = (e) => {
        console.error('Error reading file:', e);
        setError(`Error reading file ${file.name}: ${e.target.error.message}`);
        setIsLoading(false);
      }
      reader.readAsText(file);
    }
  };

  const renderView = () => {
    if (isLoading) {
      return html`<div aria-busy="true">Loading prompts...</div>`;
    }
    if (error) {
      return html`<div class="error-message" role="alert">
        <p><strong>Error:</strong> ${error}</p>
        <p>Please ensure the Markdown file is correctly formatted or try selecting another file.</p>
      </div>`;
    }
     if (!promptsData || promptsData.length === 0 && !isLoading) {
      return html`<p>No prompts loaded. Try selecting a Markdown file or check the console for errors.</p>`;
    }


    const parts = currentPath.split('/').filter(Boolean);

    if (parts[0] === 'prompt' && parts[1]) {
      const promptId = decodeURIComponent(parts[1]);
      // Find prompt by ID. ID is category_title_index
      let foundPrompt = null;
      let categoryOfPrompt = null;
      for (const category of promptsData) {
        const prompt = category.prompts.find(p => p.id === promptId);
        if (prompt) {
          foundPrompt = prompt;
          categoryOfPrompt = category;
          break;
        }
      }
      
      if (foundPrompt) {
        return html`<${PromptDetailView} 
          prompt=${foundPrompt} 
          categoryName=${categoryOfPrompt.name}
          onNavigate=${navigate}
          onShowToast=${showToast}
        />`;
      } else {
         return html`<div>Prompt not found. <a href="#" onClick=${(e) => { e.preventDefault(); navigate('/'); }}>Go to Categories</a></div>`;
      }
    }

    if (parts[0] === 'category' && parts[1]) {
      const categoryName = decodeURIComponent(parts[1]);
      const category = promptsData.find(cat => cat.name === categoryName);
      if (category) {
        return html`<${PromptListView} 
          category=${category} 
          onNavigate=${navigate} 
        />`;
      } else {
        return html`<div>Category not found. <a href="#" onClick=${(e) => { e.preventDefault(); navigate('/'); }}>Go to Categories</a></div>`;
      }
    }
    
    // Default to CategoryView
    return html`<${CategoryView} categories=${promptsData} onNavigate=${navigate} />`;
  };

  return html`
    <${Header} onFileChange=${handleFileChange} onNavigate=${navigate} />
    ${renderView()}
    ${toastMessage && html`<div class="toast show">${toastMessage}</div>`}
  `;
};

export default App;