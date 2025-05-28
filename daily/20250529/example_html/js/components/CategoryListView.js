// js/components/CategoryListView.js
import { h } from 'preact';
import { html } from 'htm/preact';

function CategoryListView({ categories, onCategorySelect }) {
  if (!categories || categories.length === 0) {
    return html`<p>No categories available.</p>`;
  }

  return html`
    <section id="category-list-view">
      <h2>カテゴリを選択</h2>
      <div id="category-list-container">
        <ul role="list">
          ${categories.map(category => html`
            <li key=${category.categoryName}>
              <button 
                class="outline list-button" 
                onClick=${() => onCategorySelect(category.categoryName)}
              >
                ${category.categoryName}
              </button>
            </li>
          `)}
        </ul>
      </div>
    </section>
  `;
}

export default CategoryListView;