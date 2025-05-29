import { h } from 'preact';
import htm from 'htm';

const html = htm.bind(h);

const PromptListView = ({ category, onNavigate }) => {
  if (!category) {
    return html`<p>カテゴリ情報がありません。</p>`;
  }

  return html`
    <section>
      <button class="secondary outline back-button" onClick=${() => onNavigate('/')}>
        &lt; カテゴリ一覧へ戻る
      </button>
      <hgroup>
        <h2>${category.name}</h2>
        <p>利用したいプロンプトを選択してください。</p>
      </hgroup>
      ${category.prompts.map(prompt => html`
        <article 
          class="prompt-card" 
          key=${prompt.id} 
          onClick=${() => onNavigate(`/prompt/${encodeURIComponent(prompt.id)}`)}
          style=${{ cursor: 'pointer' }}
          tabindex="0"
          role="button"
          onKeyPress=${(e) => e.key === 'Enter' && onNavigate(`/prompt/${encodeURIComponent(prompt.id)}`)}
        >
          <hgroup>
            <h3>${prompt.title}</h3>
            ${prompt.description && html`<p>${prompt.description.split('\n')[0]}</p>`} <!-- Show first line of desc -->
          </hgroup>
        </article>
      `)}
    </section>
  `;
};

export default PromptListView;