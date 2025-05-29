export const parseMarkdown = (markdownContent) => {
  if (typeof markdownContent !== 'string') {
    console.error("Invalid markdownContent: not a string", markdownContent);
    return [];
  }
  const lines = markdownContent.split('\n');
  const categories = [];
  let currentCategory = null;
  let currentPrompt = null;
  let parsingDescription = false;
  let parsingTemplate = false;
  let templateLines = [];
  let promptCounter = 0; // For unique IDs

  for (const line of lines) {
    const categoryMatch = line.match(/^#\s+(.+)/);
    if (categoryMatch) {
      if (currentCategory) {
        if (currentPrompt) {
          if (parsingTemplate) currentPrompt.template = templateLines.join('\n').trim();
          currentCategory.prompts.push(currentPrompt);
          currentPrompt = null;
          templateLines = [];
          parsingTemplate = false;
        }
        categories.push(currentCategory);
      }
      currentCategory = { name: categoryMatch[1].trim(), prompts: [] };
      parsingDescription = false;
      continue;
    }

    if (!currentCategory) continue; // Skip lines until a category is found

    const promptTitleMatch = line.match(/^##\s+(.+)/);
    if (promptTitleMatch) {
      if (currentPrompt) {
        if (parsingTemplate) currentPrompt.template = templateLines.join('\n').trim();
        currentCategory.prompts.push(currentPrompt);
        templateLines = [];
      }
      promptCounter++;
      currentPrompt = {
        id: `${currentCategory.name.replace(/\s+/g, '_')}_${promptTitleMatch[1].trim().replace(/\s+/g, '_')}_${promptCounter}`,
        title: promptTitleMatch[1].trim(),
        description: '',
        template: '',
        variables: []
      };
      parsingDescription = true;
      parsingTemplate = false;
      continue;
    }

    if (currentPrompt) {
      if (line.trim() === '````') {
        if (parsingTemplate) { // End of template
          currentPrompt.template = templateLines.join('\n').trim();
          // Extract variables
          const variableRegex = /\{\{\s*([\w_]+)\s*\}\}/g;
          let match;
          const variables = new Set();
          while ((match = variableRegex.exec(currentPrompt.template)) !== null) {
            variables.add(match[1]);
          }
          currentPrompt.variables = Array.from(variables);
          
          // currentCategory.prompts.push(currentPrompt); // Push when next prompt or category starts, or EOF
          // currentPrompt = null; // Reset after processing
          templateLines = [];
          parsingTemplate = false;
          parsingDescription = false; // Description ends when template starts or ends
        } else { // Start of template
          parsingTemplate = true;
          parsingDescription = false; // Description ends when template starts
          // If there was a description, it should already be captured
        }
        continue;
      }

      if (parsingTemplate) {
        templateLines.push(line);
      } else if (parsingDescription && line.trim() !== '') {
        // Only add to description if it's not empty and not start/end of template
        currentPrompt.description += (currentPrompt.description ? '\n' : '') + line.trim();
      } else if (parsingDescription && line.trim() === '' && currentPrompt.description !== ''){
        // Allow blank lines within description if description already has content
         currentPrompt.description += '\n';
      }
    }
  }

  // Add the last processed category and prompt
  if (currentPrompt) {
     if (parsingTemplate) currentPrompt.template = templateLines.join('\n').trim();
     if (currentPrompt.template && !currentPrompt.variables.length) { // Re-extract if template was finalized at EOF
        const variableRegex = /\{\{\s*([\w_]+)\s*\}\}/g;
        let match;
        const variables = new Set();
        while ((match = variableRegex.exec(currentPrompt.template)) !== null) {
          variables.add(match[1]);
        }
        currentPrompt.variables = Array.from(variables);
     }
    currentCategory.prompts.push(currentPrompt);
  }
  if (currentCategory) {
    categories.push(currentCategory);
  }
  
  // Filter out categories with no prompts (e.g. if parsing failed mid-category)
  return categories.filter(cat => cat.prompts.length > 0);
};