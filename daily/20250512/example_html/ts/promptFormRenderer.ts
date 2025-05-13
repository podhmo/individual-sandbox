// ts/promptFormRenderer.ts
import { Template } from "./data";

/**
 * プロンプト生成フォームの表示と処理を担当するモジュール
 */
export class PromptFormRenderer {
  private formContainer: HTMLElement;
  private templateTitleElement: HTMLElement;
  private templateBodyPreviewElement: HTMLElement;
  private templateDescriptionElement: HTMLElement;
  private variableInputsContainer: HTMLElement;
  private generatedPromptTextarea: HTMLTextAreaElement;
  private copyButton: HTMLButtonElement;
  private feedbackMessageElement: HTMLElement;

  constructor(
    formContainerId: string,
    templateTitleId: string,
    templateBodyPreviewId: string,
    templateDescriptionId: string,
    variableInputsContainerId: string,
    generatedPromptTextareaId: string,
    copyButtonId: string,
    feedbackMessageId: string,
  ) {
    this.formContainer = document.getElementById(formContainerId)!;
    this.templateTitleElement = document.getElementById(templateTitleId)!;
    this.templateBodyPreviewElement = document.getElementById(
      templateBodyPreviewId,
    )!;
    this.templateDescriptionElement = document.getElementById(
      templateDescriptionId,
    )!;
    this.variableInputsContainer = document.getElementById(
      variableInputsContainerId,
    )!;
    this.generatedPromptTextarea = document.getElementById(
      generatedPromptTextareaId,
    )! as HTMLTextAreaElement;
    this.copyButton = document.getElementById(
      copyButtonId,
    )! as HTMLButtonElement;
    this.feedbackMessageElement = document.getElementById(feedbackMessageId)!;
  }

  /**
   * 選択されたテンプレートに基づいてプロンプト生成フォームを描画
   * @param template - 選択されたテンプレートオブジェクト
   */
  renderPromptForm(template: Template): void {
    this.templateTitleElement.textContent = template.name;
    this.templateBodyPreviewElement.textContent = template.template;

    // Descriptionの処理
    if (template.description) {
      this.templateDescriptionElement.textContent = template.description;
    }

    this.variableInputsContainer.innerHTML = ""; // 既存の入力フィールドをクリア
    this.generatedPromptTextarea.value = ""; // 生成済みプロンプトをクリア
    this.feedbackMessageElement.textContent = ""; // フィードバックメッセージをクリア
    this.copyButton.disabled = true;

    template.variables.forEach((variableName) => {
      const label = document.createElement("label");
      label.htmlFor = `var-${variableName}`;
      label.textContent = `${variableName}:`;

      const input = document.createElement("input");
      input.type = "text";
      input.id = `var-${variableName}`;
      input.name = variableName;
      input.placeholder = `例: ${variableName}を入力`;
      input.setAttribute("aria-label", variableName);
      input.addEventListener(
        "input",
        () => this.updateGeneratedPrompt(template),
      ); // 入力時にプレビュー更新

      this.variableInputsContainer.appendChild(label);
      this.variableInputsContainer.appendChild(input);
    });

    this.copyButton.onclick = async () => {
      if (this.generatedPromptTextarea.value) {
        try {
          await navigator.clipboard.writeText(
            this.generatedPromptTextarea.value,
          );
          this.feedbackMessageElement.textContent =
            "クリップボードにコピーしました！";
          this.feedbackMessageElement.style.color = "var(--pico-primary-hover)"; // より目立つようにhover色を使ってみる (要確認)
          setTimeout(() => this.feedbackMessageElement.textContent = "", 3000);
        } catch (err) {
          console.error("クリップボードへのコピーに失敗しました:", err);
          this.feedbackMessageElement.textContent = "コピーに失敗しました。";
          this.feedbackMessageElement.style.color =
            "var(--pico-form-invalid-border-color)"; // フォームの無効なボーダー色 (赤系)
        }
      }
    };
    this.updateGeneratedPrompt(template); // 初期プロンプト表示
  }

  /**
   * 入力値に基づいて生成済みプロンプトを更新
   * @param template - 現在のテンプレートオブジェクト
   */
  private updateGeneratedPrompt(template: Template): void {
    let prompt = template.template;
    let allVariablesFilled = true;
    template.variables.forEach((variableName) => {
      const inputElement = document.getElementById(
        `var-${variableName}`,
      ) as HTMLInputElement;
      if (inputElement) {
        const value = inputElement.value;
        if (value.trim() === "") {
          // 空の場合はそのまま{{変数名}}を残すか、何かプレースホルダを入れるか。
          // ここでは簡単のため、未入力の場合は置換しないでおく。
          // allVariablesFilled = false; // もし全て埋まってないとコピー不可にするなら
        }
        // 正規表現でグローバル置換 (gフラグ)
        prompt = prompt.replace(
          new RegExp(`\\{\\{${variableName}\\}\\}`, "g"),
          value || `{{${variableName}}}`,
        );
      }
    });
    this.generatedPromptTextarea.value = prompt;
    this.copyButton.disabled = this.generatedPromptTextarea.value.trim() === "";
  }
}
