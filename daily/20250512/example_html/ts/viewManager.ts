// ts/viewManager.ts

/**
 * アプリケーションの表示状態を管理するモジュール
 */
export class ViewManager {
  private categoryListView: HTMLElement;
  private templateListView: HTMLElement;
  private promptFormView: HTMLElement;

  constructor(
    categoryListElementId: string,
    templateListElementId: string,
    promptFormElementId: string
  ) {
    this.categoryListView = document.getElementById(categoryListElementId)!;
    this.templateListView = document.getElementById(templateListElementId)!;
    this.promptFormView = document.getElementById(promptFormElementId)!;
    this.showCategoryListView(); // 初期表示
  }

  /**
   * カテゴリ一覧画面を表示
   */
  showCategoryListView(): void {
    this.categoryListView.style.display = 'block';
    this.templateListView.style.display = 'none';
    this.promptFormView.style.display = 'none';
  }

  /**
   * テンプレート一覧画面を表示
   */
  showTemplateListView(): void {
    this.categoryListView.style.display = 'none';
    this.templateListView.style.display = 'block';
    this.promptFormView.style.display = 'none';
  }

  /**
   * プロンプト生成フォーム画面を表示
   */
  showPromptFormView(): void {
    this.categoryListView.style.display = 'none';
    this.templateListView.style.display = 'none';
    this.promptFormView.style.display = 'block';
  }
}