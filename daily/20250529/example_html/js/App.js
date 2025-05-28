// js/App.js
import { h } from 'preact';
import { useState, useEffect, useCallback } from 'preact/hooks';
import { html } from 'htm/preact';

import CategoryListView from './components/CategoryListView.js';
import TemplateListView from './components/TemplateListView.js';
import PromptFormView from './components/PromptFormView.js';
import Breadcrumbs from './components/Breadcrumbs.js';
import LoadingSpinner from './components/LoadingSpinner.js';

const VIEWS = {
  CATEGORY_LIST: 'categoryList',
  TEMPLATE_LIST: 'templateList',
  PROMPT_FORM: 'promptForm',
};

function App({ initialTemplates }) {
  const [hierarchicalTemplates, setHierarchicalTemplates] = useState(initialTemplates || []);
  const [isLoading, setIsLoading] = useState(!initialTemplates);
  const [error, setError] = useState(null);

  const [currentView, setCurrentView] = useState(VIEWS.CATEGORY_LIST);
  const [selectedCategoryName, setSelectedCategoryName] = useState(null);
  const [selectedTemplateId, setSelectedTemplateId] = useState(null);

  useEffect(() => {
    if (!initialTemplates) {
      // js/main.js で初期データが渡されることを期待
      setIsLoading(false); 
    } else {
      setIsLoading(false);
    }
  }, [initialTemplates]);

  const handleCategorySelect = useCallback((categoryName) => {
    setSelectedCategoryName(categoryName);
    setCurrentView(VIEWS.TEMPLATE_LIST);
    setSelectedTemplateId(null); // カテゴリ変更時にテンプレート選択をリセット
  }, []);

  const handleTemplateSelect = useCallback((templateId, categoryName) => {
    setSelectedTemplateId(templateId);
    if (categoryName && categoryName !== selectedCategoryName) {
        setSelectedCategoryName(categoryName); 
    }
    setCurrentView(VIEWS.PROMPT_FORM);
  }, [selectedCategoryName]);

  const handleBackToCategories = useCallback(() => {
    setCurrentView(VIEWS.CATEGORY_LIST);
    setSelectedCategoryName(null);
    setSelectedTemplateId(null);
  }, []);

  const handleBackToTemplates = useCallback(() => {
    setCurrentView(VIEWS.TEMPLATE_LIST);
    setSelectedTemplateId(null); 
  }, []);


  if (isLoading) {
    return html`<${LoadingSpinner} />`;
  }
  if (error) {
    return html`<p>Error: ${error}</p>`;
  }
  if (!hierarchicalTemplates || hierarchicalTemplates.length === 0) {
      return html`<p>No templates found. Check your templates.md file or network issues.</p>`;
  }
  
  const selectedCategory = selectedCategoryName 
    ? hierarchicalTemplates.find(cat => cat.categoryName === selectedCategoryName) 
    : null;
  
  const selectedTemplate = selectedCategory && selectedTemplateId
    ? selectedCategory.templates.find(tpl => tpl.id === selectedTemplateId)
    : null;

  return html`
    <div>
      <${Breadcrumbs} 
        currentView=${currentView}
        selectedCategoryName=${selectedCategoryName}
        selectedTemplateName=${selectedTemplate?.name}
        onNavigateToCategories=${handleBackToCategories}
        onNavigateToTemplates=${handleBackToTemplates}
      />

      ${currentView === VIEWS.CATEGORY_LIST && html`
        <${CategoryListView} 
          categories=${hierarchicalTemplates} 
          onCategorySelect=${handleCategorySelect} 
        />
      `}
      ${currentView === VIEWS.TEMPLATE_LIST && selectedCategory && html`
        <${TemplateListView} 
          category=${selectedCategory}
          onTemplateSelect=${handleTemplateSelect}
          onBackToCategories=${handleBackToCategories}
        />
      `}
      ${currentView === VIEWS.PROMPT_FORM && selectedTemplate && selectedCategory && html`
        <${PromptFormView} 
          template=${selectedTemplate}
          categoryName=${selectedCategory.categoryName}
          onBackToTemplates=${handleBackToTemplates}
        />
      `}
    </div>
  `;
}

export default App;