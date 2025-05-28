// js/components/LoadingSpinner.js
import { h } from 'preact';
import { html } from 'htm/preact';

function LoadingSpinner() {
  return html`
    <div class="loading-spinner" aria-label="読み込み中">
      データを読み込んでいます
      <!-- HTML コメント -->
    </div>
  `;
}

export default LoadingSpinner;