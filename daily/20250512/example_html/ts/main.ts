// ts/main.ts
import { HIERARCHICAL_TEMPLATES, CategoryTemplates, Template } from './data';
import { ViewManager } from './viewManager';
import { TemplateListRenderer } from './templateListRenderer';
import { PromptFormRenderer } from './promptFormRenderer';

document.addEventListener('DOMContentLoaded', () => {
  // 各画面のコンテナID
  const CATEGORY_LIST_VIEW_ID = 'category-list-view';
  const TEMPLATE_LIST_VIEW_ID = 'template-list-view';
  const PROMPT_FORM_VIEW_ID = 'prompt-form-view';

  // TemplateListRendererで使用するコンテナID
  const CATEGORY_LIST_CONTAINER_ID = 'category-list-container';
  const TEMPLATE_LIST_CONTAINER_ID = 'template-list-container';
  const TEMPLATE_LIST_TITLE_ID = 'template-list-title'; // カテゴリ名表示用

  // PromptFormRendererで使用する要素のID
  const PROMPT_FORM_CONTAINER_ID = 'prompt-form-container'; // フォーム全体のコンテナ
  const TEMPLATE_TITLE_ID = 'prompt-template-title';
  const TEMPLATE_BODY_PREVIEW_ID = 'prompt-template-body-preview';
  const VARIABLE_INPUTS_CONTAINER_ID = 'prompt-variable-inputs';
  const GENERATED_PROMPT_TEXTAREA_ID = 'prompt-generated-textarea';
  const COPY_BUTTON_ID = 'prompt-copy-button';
  const FEEDBACK_MESSAGE_ID = 'prompt-feedback-message';
  const BACK_TO_CATEGORY_LIST_BUTTON_ID = 'back-to-category-list';
  const BACK_TO_TEMPLATE_LIST_BUTTON_ID = 'back-to-template-list';


  // インスタンス化
  const viewManager = new ViewManager(
    CATEGORY_LIST_VIEW_ID,
    TEMPLATE_LIST_VIEW_ID,
    PROMPT_FORM_VIEW_ID
  );

  const templateListRenderer = new TemplateListRenderer(
    CATEGORY_LIST_CONTAINER_ID,
    TEMPLATE_LIST_CONTAINER_ID,
    TEMPLATE_LIST_TITLE_ID
  );

  const promptFormRenderer = new PromptFormRenderer(
    PROMPT_FORM_CONTAINER_ID,
    TEMPLATE_TITLE_ID,
    TEMPLATE_BODY_PREVIEW_ID,
    VARIABLE_INPUTS_CONTAINER_ID,
    GENERATED_PROMPT_TEXTAREA_ID,
    COPY_BUTTON_ID,
    FEEDBACK_MESSAGE_ID
  );

  let currentSelectedCategoryName: string | null = null;

  // カテゴリ選択時の処理
  const handleCategorySelect = (categoryName: string) => {
    currentSelectedCategoryName = categoryName;
    const category = HIERARCHICAL_TEMPLATES.find(c => c.categoryName === categoryName);
    if (category) {
      templateListRenderer.renderTemplateList(category.categoryName, category.templates, handleTemplateSelect);
      viewManager.showTemplateListView();
    }
  };

  // テンプレート選択時の処理
  const handleTemplateSelect = (templateId: string, categoryName: string) => {
    const category = HIERARCHICAL_TEMPLATES.find(c => c.categoryName === categoryName);
    const template = category?.templates.find(t => t.id === templateId);
    if (template) {
      promptFormRenderer.renderPromptForm(template);
      viewManager.showPromptFormView();
    }
  };

  // 初期表示: カテゴリ一覧
  templateListRenderer.renderCategoryList(HIERARCHICAL_TEMPLATES, handleCategorySelect);

  // 戻るボタンのイベントリスナー
  document.getElementById(BACK_TO_CATEGORY_LIST_BUTTON_ID)!.addEventListener('click', () => {
    viewManager.showCategoryListView();
  });
  document.getElementById(BACK_TO_TEMPLATE_LIST_BUTTON_ID)!.addEventListener('click', () => {
    if (currentSelectedCategoryName) {
        // 再度テンプレートリストを表示するためにカテゴリデータを渡す必要がある
        const category = HIERARCHICAL_TEMPLATES.find(c => c.categoryName === currentSelectedCategoryName);
        if (category) {
            templateListRenderer.renderTemplateList(category.categoryName, category.templates, handleTemplateSelect);
            viewManager.showTemplateListView();
        } else {
            // 万が一カテゴリが見つからない場合はカテゴリ一覧へ
            viewManager.showCategoryListView();
        }
    } else {
        viewManager.showCategoryListView(); // カテゴリが選択されていなければカテゴリ一覧へ
    }
  });

});