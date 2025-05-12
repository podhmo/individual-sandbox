// ts/templateListRenderer.ts
import { CategoryTemplates, Template } from './data';

/**
 * カテゴリ一覧とテンプレート一覧の表示を担当するモジュール
 */
export class TemplateListRenderer {
  private categoryListContainer: HTMLElement;
  private templateListContainer: HTMLElement;
  private templateListTitle: HTMLElement; // テンプレート一覧のタイトル（カテゴリ名表示用）

  constructor(
    categoryListContainerId: string,
    templateListContainerId: string,
    templateListTitleId: string
  ) {
    this.categoryListContainer = document.getElementById(categoryListContainerId)!;
    this.templateListContainer = document.getElementById(templateListContainerId)!;
    this.templateListTitle = document.getElementById(templateListTitleId)!;
  }

  /**
   * カテゴリ一覧を描画
   * @param categories - 表示するカテゴリデータの配列
   * @param onCategorySelect - カテゴリ選択時のコールバック関数 (選択されたカテゴリ名を引数に取る)
   */
  renderCategoryList(
    categories: CategoryTemplates[],
    onCategorySelect: (categoryName: string) => void
  ): void {
    this.categoryListContainer.innerHTML = ''; // 既存の内容をクリア
    const list = document.createElement('ul');
    list.setAttribute('role', 'list');

    categories.forEach(category => {
      const listItem = document.createElement('li');
      const button = document.createElement('button');
      button.textContent = category.categoryName;
      button.classList.add('outline'); // Pico.css のスタイル
      button.style.width = '100%'; // 幅を広げる
      button.style.marginBottom = 'var(--pico-spacing)'; // ボタン間のマージン
      button.addEventListener('click', () => onCategorySelect(category.categoryName));
      listItem.appendChild(button);
      list.appendChild(listItem);
    });
    this.categoryListContainer.appendChild(list);
  }

  /**
   * 特定のカテゴリに属するテンプレート一覧を描画
   * @param categoryName - 表示するカテゴリ名
   * @param templates - 表示するテンプレートデータの配列
   * @param onTemplateSelect - テンプレート選択時のコールバック関数 (選択されたテンプレートIDを引数に取る)
   */
  renderTemplateList(
    categoryName: string,
    templates: Template[],
    onTemplateSelect: (templateId: string, categoryName: string) => void
  ): void {
    this.templateListTitle.textContent = `カテゴリ: ${categoryName}`;
    this.templateListContainer.innerHTML = ''; // 既存の内容をクリア（タイトルは残すので注意）

    const list = document.createElement('ul');
    list.setAttribute('role', 'list');

    templates.forEach(template => {
      const listItem = document.createElement('li');
      const button = document.createElement('button');
      button.textContent = template.name;
      button.classList.add('outline'); // Pico.css のスタイル
      button.style.width = '100%'; // 幅を広げる
      button.style.marginBottom = 'var(--pico-spacing)'; // ボタン間のマージン
      button.addEventListener('click', () => onTemplateSelect(template.id, categoryName));
      listItem.appendChild(button);
      list.appendChild(listItem);
    });
    this.templateListContainer.appendChild(list);
  }
}