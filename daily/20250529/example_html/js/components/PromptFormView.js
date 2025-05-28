// js/components/PromptFormView.js
import { h } from 'preact';
import { useState, useEffect, useCallback } from 'preact/hooks';
import { html } from 'htm/preact';

function PromptFormView({ template, categoryName, onBackToTemplates }) {
  const [variableValues, setVariableValues] = useState({});
  const [generatedPrompt, setGeneratedPrompt] = useState('');
  const [feedbackMessage, setFeedbackMessage] = useState('');
  const [isCopyButtonDisabled, setIsCopyButtonDisabled] = useState(true);

  useEffect(() => {
    // テンプレートが変更されたら変数の初期値を設定
    const initialValues = {};
    template.variables.forEach(varName => {
      initialValues[varName] = '';
    });
    setVariableValues(initialValues);
  }, [template]);

  useEffect(() => {
    // 変数値が変更されたらプロンプトを再生成
    let prompt = template.template;
    let allFilled = true; // 今回は使わないが、将来的に全入力必須にするなら使える
    template.variables.forEach(varName => {
      const value = variableValues[varName] || '';
      // if (value.trim() === '') allFilled = false;
      prompt = prompt.replace(new RegExp(`\\{\\{${varName}\\}\\}`, 'g'), value || `{{${varName}}}`);
    });
    setGeneratedPrompt(prompt);
    setIsCopyButtonDisabled(prompt.trim() === '');
  }, [template, variableValues]);

  const handleInputChange = useCallback((varName, value) => {
    setVariableValues(prev => ({ ...prev, [varName]: value }));
  }, []);

  const handleCopyPrompt = useCallback(async () => {
    if (generatedPrompt) {
      try {
        await navigator.clipboard.writeText(generatedPrompt);
        setFeedbackMessage('クリップボードにコピーしました！');
        setTimeout(() => setFeedbackMessage(''), 3000);
      } catch (err) {
        console.error('クリップボードへのコピーに失敗しました:', err);
        setFeedbackMessage('コピーに失敗しました。');
      }
    }
  }, [generatedPrompt]);

  if (!template) {
    return html`<p>Template not found.</p>`;
  }

  return html`
    <section id="prompt-form-view">
      {/* パンくずはApp.jsで管理 */}
      <article id="prompt-form-container">
        <h2 id="prompt-template-title">${template.name}</h2>
        <details>
          <summary>テンプレート本文を確認</summary>
          <p class="template-preview">${template.template}</p>
        </details>
        
        <form onSubmit=${(e) => e.preventDefault()}>
          <div id="prompt-variable-inputs">
            ${template.variables.map(varName => html`
              <label htmlFor="var-${varName}">${varName}:</label>
              <input 
                type="text" 
                id="var-${varName}" 
                name=${varName}
                placeholder="例: ${varName}を入力"
                aria-label=${varName}
                value=${variableValues[varName] || ''}
                onInput=${(e) => handleInputChange(varName, e.target.value)}
              />
            `)}
          </div>
          
          <label htmlFor="prompt-generated-textarea">生成されたプロンプト:</label>
          <textarea 
            id="prompt-generated-textarea" 
            rows="8" 
            aria-label="生成されたプロンプト" 
            readonly
            value=${generatedPrompt}
          ></textarea>
          
          <button 
            type="button" 
            id="prompt-copy-button" 
            disabled=${isCopyButtonDisabled}
            onClick=${handleCopyPrompt}
          >
            クリップボードにコピー
          </button>
          ${feedbackMessage && html`
            <small id="prompt-feedback-message" class="feedback-message" aria-live="polite">
              ${feedbackMessage}
            </small>
          `}
        </form>

        ${template.description && html`
            <h3>テンプレートの説明</h3>
            <pre id="prompt-template-description">${template.description}</pre>
        `}
      </article>
    </section>
  `;
}

export default PromptFormView;