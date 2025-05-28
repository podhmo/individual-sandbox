// js/components/Breadcrumbs.js
import { h } from 'preact';
import { html } from 'htm/preact';

const VIEWS = {
  CATEGORY_LIST: 'categoryList',
  TEMPLATE_LIST: 'templateList',
  PROMPT_FORM: 'promptForm',
};

function Breadcrumbs({ currentView, selectedCategoryName, selectedTemplateName, onNavigateToCategories, onNavigateToTemplates }) {
  return html`
    <nav aria-label="breadcrumb">
      <ul>
        ${currentView !== VIEWS.CATEGORY_LIST && html`
          <li>
            <button class="secondary outline" onClick=${onNavigateToCategories}>
              カテゴリ一覧
            </button>
          </li>
        `}
        ${currentView === VIEWS.PROMPT_FORM && selectedCategoryName && html`
          <li>
            <button class="secondary outline" onClick=${onNavigateToTemplates}>
              ${selectedCategoryName}
            </button>
          </li>
        `}
        ${currentView === VIEWS.PROMPT_FORM && selectedTemplateName && html`
          <li><span aria-current="page">${selectedTemplateName}</span></li>
        `}
      </ul>
    </nav>
  `;
}

export default Breadcrumbs;