// js/components/TemplateListView.js
import { h } from 'preact';
import { html } from 'htm/preact';

function TemplateListView({ category, onTemplateSelect, onBackToCategories }) {
  if (!category) {
    return html`<p>Category not found.</p>`;
  }
  if (!category.templates || category.templates.length === 0) {
     // onBackToCategories() // カテゴリに戻る処理を自動実行してもよい
    return html`
      <p>No templates found in this category: ${category.categoryName}</p>
      <button class="secondary outline" onClick=${onBackToCategories}>カテゴリ一覧へ戻る</button>
    `;
  }

  return html`
    <section id="template-list-view">
      {/* パンくずはApp.jsで管理するためここでは不要 */}
      <h2 id="template-list-title">カテゴリ: ${category.categoryName}</h2>
      <div id="template-list-container">
        <ul role="list">
          ${category.templates.map(template => html`
            <li key=${template.id}>
              <button 
                class="outline list-button" 
                onClick=${() => onTemplateSelect(template.id, category.categoryName)}
              >
                ${template.name}
              </button>
            </li>
          `)}
        </ul>
      </div>
    </section>
  `;
}

export default TemplateListView;