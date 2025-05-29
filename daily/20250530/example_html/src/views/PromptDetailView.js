import { h } from 'preact';
import { useState, useEffect } from 'preact/hooks';
import htm from 'htm';
import { processTemplate } from '../utils/templateEngine.js';

const html = htm.bind(h);

const PromptDetailView = ({ prompt, categoryName, onNavigate, onShowToast }) => {
  if (!prompt) {
    return html`<p>プロンプト情報がありません。</p>`;
  }

  const [variableValues, setVariableValues] = useState({});
  const [generatedPrompt, setGeneratedPrompt] = useState('');

  useEffect(() => {
    // Initialize variableValues with empty strings for each variable
    const initialValues = {};
    if (prompt.variables) {
      prompt.variables.forEach(v => initialValues[v] = '');
    }
    setVariableValues(initialValues);
  }, [prompt]);

  useEffect(() => {
    if (prompt.template) {
      setGeneratedPrompt(processTemplate(prompt.template, variableValues));
    }
  }, [prompt.template, variableValues]);

  const handleInputChange = (variableName, value) => {
    setVariableValues(prev => ({ ...prev, [variableName]: value }));
  };

  const handleCopyToClipboard = async () => {
    if (!navigator.clipboard) {
      onShowToast('クリップボード機能が利用できません。');
      console.error('Clipboard API not available');
      return;
    }
    try {
      await navigator.clipboard.writeText(generatedPrompt);
      onShowToast('プロンプトをコピーしました！');
    } catch (err) {
      onShowToast('コピーに失敗しました。');
      console.error('Failed to copy text: ', err);
    }
  };

  return html`
    <section>
      <button 
        class="secondary outline back-button" 
        onClick=${() => onNavigate(`/category/${encodeURIComponent(categoryName)}`)}
      >
        &lt; 「${categoryName}」のプロンプト一覧へ戻る
      </button>
      <article class="prompt-card">
        <hgroup>
          <h2>${prompt.title}</h2>
          ${prompt.description && html`<p>${prompt.description.split('\n').map(line => html`${line}<br />`)}</p>`}
        </hgroup>

        ${prompt.variables && prompt.variables.length > 0 && html`
          <form onSubmit=${(e) => e.preventDefault()}>
            <h4>変数を入力:</h4>
            ${prompt.variables.map(variable => html`
              <div class="variable-input" key=${variable}>
                <label htmlFor=${`var-${variable}`}>${variable}:</label>
                ${ (prompt.template.split('\n').length > 2 && variable.toLowerCase().includes('text')) || variable.toLowerCase().includes('description') || variable.toLowerCase().includes('content') ?
                  html`<textarea
                    id=${`var-${variable}`}
                    rows="3"
                    value=${variableValues[variable] || ''}
                    onInput=${(e) => handleInputChange(variable, e.target.value)}
                    placeholder="ここに ${variable} を入力..."
                  />` :
                  html`<input
                    type="text"
                    id=${`var-${variable}`}
                    value=${variableValues[variable] || ''}
                    onInput=${(e) => handleInputChange(variable, e.target.value)}
                    placeholder="ここに ${variable} を入力..."
                  />`
                }
              </div>
            `)}
          </form>
        `}
        ${!prompt.variables || prompt.variables.length === 0 && html`
            <p><em>このプロンプトには編集可能な変数はありません。</em></p>
        `}
        
        <h4>生成されるプロンプト:</h4>
        <div class="prompt-template-display">
          ${generatedPrompt.split('\n').map(line => html`${line}<br />`)}
        </div>

        <button class="copy-button" onClick=${handleCopyToClipboard}>
          クリップボードにコピー
        </button>
      </article>
    </section>
  `;
};

export default PromptDetailView;