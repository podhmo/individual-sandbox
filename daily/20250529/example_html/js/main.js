// js/main.js
import { render, h } from 'preact';
import { html } from 'htm/preact';
import App from './App.js';
import { parseMarkdownToHierarchicalTemplates } from './parser.js';
import LoadingSpinner from './components/LoadingSpinner.js';

const appRoot = document.getElementById('app');

async function main() {
  render(html`<${LoadingSpinner} />`, appRoot); // 初期ローディング表示

  try {
    const response = await fetch('/templates.md');
    if (!response.ok) {
      throw new Error(`Failed to fetch templates: ${response.statusText}`);
    }
    const markdown = await response.text();
    const hierarchicalTemplates = parseMarkdownToHierarchicalTemplates(markdown);
    
    render(html`<${App} initialTemplates=${hierarchicalTemplates} />`, appRoot);
  } catch (error) {
    console.error("Error loading templates:", error);
    render(html`<p>Error loading templates. Please check the console for details.</p>`, appRoot);
  }
}

main();