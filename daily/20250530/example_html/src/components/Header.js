import { h } from 'preact';
import htm from 'htm';

const html = htm.bind(h);

const Header = ({ onFileChange, onNavigate }) => {
  return html`
    <header style=${{ marginBottom: '1.5rem' }}>
      <nav>
        <ul>
          <li>
            <strong onClick=${() => onNavigate('/')} style=${{cursor: 'pointer'}}>
              Markdown プロンプトアシスタント
            </strong>
          </li>
        </ul>
        <ul>
          <li>
            <details role="list" dir="rtl">
              <summary aria-haspopup="listbox" role="button" class="contrast">ファイル</summary>
              <ul role="listbox">
                <li>
                  <label htmlFor="fileInput" style=${{cursor: 'pointer', display: 'block', padding: '0.5rem 1rem'}}>
                    Markdown読込...
                    <input type="file" id="fileInput" accept=".md, .markdown" onChange=${onFileChange} style=${{ display: 'none' }} />
                  </label>
                </li>
              </ul>
            </details>
          </li>
        </ul>
      </nav>
    </header>
  `;
};

export default Header;