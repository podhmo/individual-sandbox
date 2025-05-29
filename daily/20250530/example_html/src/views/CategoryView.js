import { h } from 'preact';
import htm from 'htm';

const html = htm.bind(h);

const CategoryView = ({ categories, onNavigate }) => {
  if (!categories || categories.length === 0) {
    return html`<p>利用可能なカテゴリがありません。Markdownファイルを確認または選択してください。</p>`;
  }

  return html`
    <section>
      <hgroup>
        <h2>カテゴリ一覧</h2>
        <p>利用したいプロンプトのカテゴリを選択してください。</p>
      </hgroup>
      ${categories.map(category => html`
        <article 
          class="prompt-card" 
          key=${category.name} 
          onClick=${() => onNavigate(`/category/${encodeURIComponent(category.name)}`)}
          style=${{ cursor: 'pointer' }}
          tabindex="0"
          role="button"
          onKeyPress=${(e) => e.key === 'Enter' && onNavigate(`/category/${encodeURIComponent(category.name)}`)}
        >
          <h3>${category.name}</h3>
          <p>${category.prompts.length}個のプロンプト</p>
        </article>
      `)}
    </section>
  `;
};

export default CategoryView;