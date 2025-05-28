// js/parser.js

/**
 * @typedef {object} Template
 * @property {string} id - テンプレートの一意識別子
 * @property {string} name - テンプレートタイトル
 * @property {string} template - テンプレート本文 (変数を含む)
 * @property {string[]} variables - テンプレート内の変数名の配列
 * @property {string} [description] - テンプレートの説明 (オプション)
 */

/**
 * @typedef {object} CategoryTemplates
 * @property {string} categoryName - カテゴリ名
 * @property {Template[]} templates - このカテゴリに属するテンプレートの配列
 */

/**
 * テンプレート文字列から {{変数名}} 形式の変数を抽出します。
 * @param {string} templateString テンプレート本文
 * @returns {string[]} 変数名の配列 (重複なし、トリム済み)
 */
export function extractVariables(templateString) {
  const regex = /\{\{([\s\S]+?)\}\}/g;
  const matches = new Set();
  let match;
  while ((match = regex.exec(templateString)) !== null) {
    matches.add(match[1].trim());
  }
  return Array.from(matches);
}

/**
 * カテゴリ名とテンプレート名からIDを生成します。
 * URLフレンドリーな形式を目指します。
 * @param {string} categoryName カテゴリ名
 * @param {string} templateName テンプレート名
 * @returns {string} 生成されたID文字列
 */
export function generateTemplateId(categoryName, templateName) {
  const sanitize = (str) => {
    if (typeof str !== "string") return "";
    return str
      .trim()
      .toLowerCase()
      .normalize("NFD").replace(/[\u0300-\u036f]/g, "") // Remove diacritics
      .replace(/\s+/g, "-")
      .replace(/[^a-z0-9_-]+/g, "")
      .replace(/-+/g, "-")
      .replace(/^-+|-+$/g, "");
  };

  const sanCategory = sanitize(categoryName);
  const sanTemplate = sanitize(templateName);

  if (!sanCategory && !sanTemplate) {
    return `untitled-template-${Date.now().toString(36)}${Math.random().toString(36).substring(2, 7)}`;
  }
  if (!sanTemplate && sanCategory) {
    return sanCategory;
  }
  if (!sanCategory && sanTemplate) {
    return sanTemplate;
  }

  return `${sanCategory}-${sanTemplate}`;
}


/**
 * 指定されたMarkdownサブセットの文字列を解析し、CategoryTemplates[] 形式のデータを返します。
 * @param {string} markdown 解析対象のMarkdown文字列
 * @returns {CategoryTemplates[]} 解析結果の CategoryTemplates 配列
 */
export function parseMarkdownToHierarchicalTemplates(markdown) {
  const result = [];
  const lines = markdown.split(/\r?\n/);

  let currentCategory = {
    categoryName: "",
    templates: [],
  };
  let currentTemplateData = {
    name: "",
    rawDescriptionLines: [],
    templateLines: [],
    firstCodeBlockCaptured: false,
  };

  let inCodeBlock = false;
  let codeBlockFence = null;

  function finalizeCurrentTemplate() {
    if (currentTemplateData && currentCategory && currentTemplateData.name.trim()) {
      const templateBody = currentTemplateData.templateLines.join("\n");

      const descriptionLinesFiltered = currentTemplateData.rawDescriptionLines
        .map((line) => line.trimEnd())
        .filter((line, index, arr) => {
          if (
            (index === 0 && line.trim() === "") ||
            (index === arr.length - 1 && line.trim() === "" && arr.length > 1)
          ) {
            return false;
          }
          return true;
        });

      let descriptionStr = descriptionLinesFiltered.join("\n");
      if (descriptionLinesFiltered.every((l) => l.trim() === "")) {
        descriptionStr = "";
      } else if (descriptionLinesFiltered.length > 0) {
        let start = 0;
        while (
          start < descriptionLinesFiltered.length &&
          descriptionLinesFiltered[start].trim() === ""
        ) start++;
        let end = descriptionLinesFiltered.length - 1;
        while (end >= start && descriptionLinesFiltered[end].trim() === "") {
          end--;
        }
        descriptionStr = descriptionLinesFiltered.slice(start, end + 1).join("\n");
      }
      
      const description = descriptionStr ? descriptionStr : undefined;

      const variables = extractVariables(templateBody);
      const id = generateTemplateId(
        currentCategory.categoryName,
        currentTemplateData.name,
      );

      currentCategory.templates.push({
        id,
        name: currentTemplateData.name.trim(),
        template: templateBody,
        variables,
        description,
      });
    }
    currentTemplateData = {
      name: "",
      rawDescriptionLines: [],
      templateLines: [],
      firstCodeBlockCaptured: false,
    };
  }

  function startNewCategory(name) {
    finalizeCurrentTemplate(); 

    if (currentCategory && currentCategory.categoryName.trim() && currentCategory.templates.length > 0) {
      result.push(currentCategory);
    }
    
    const trimmedName = name.trim();
    if (!trimmedName) {
      currentCategory = { categoryName: "", templates: [] };
      return;
    }
    currentCategory = { categoryName: trimmedName, templates: [] };
    currentTemplateData = { name: "", rawDescriptionLines: [], templateLines: [], firstCodeBlockCaptured: false };
    inCodeBlock = false;
    codeBlockFence = null;
  }

  function startNewTemplate(name) {
    finalizeCurrentTemplate();

    const trimmedName = name.trim();
     if (!currentCategory || !trimmedName) {
      currentTemplateData = { name: "", rawDescriptionLines: [], templateLines: [], firstCodeBlockCaptured: false };
      return;
    }
    currentTemplateData = { name: trimmedName, rawDescriptionLines: [], templateLines: [], firstCodeBlockCaptured: false };
    inCodeBlock = false;
    codeBlockFence = null;
  }


  for (const line of lines) {
    if (inCodeBlock) {
      if (codeBlockFence && line.trim() === codeBlockFence) {
        inCodeBlock = false;
        codeBlockFence = null;
      } else if (currentTemplateData && currentTemplateData.firstCodeBlockCaptured) {
        currentTemplateData.templateLines.push(line);
      }
      continue;
    }

    const categoryMatch = line.match(/^#\s+(.+)/);
    if (categoryMatch) {
      startNewCategory(categoryMatch[1]);
      continue;
    }

    if (!currentCategory || !currentCategory.categoryName.trim()) { // カテゴリ名が確定するまでスキップ
        continue;
    }

    const templateMatch = line.match(/^##\s+(.+)/);
    if (templateMatch) {
      startNewTemplate(templateMatch[1]);
      continue;
    }

    if (!currentTemplateData || !currentTemplateData.name.trim()) { // テンプレート名が確定するまでスキップ
        continue;
    }
    
    const codeBlockStartMatch = line.match(/^(\`{3,}|~{3,})\s*(\S*)\s*$/);
    if (codeBlockStartMatch && currentTemplateData && !currentTemplateData.firstCodeBlockCaptured) {
      inCodeBlock = true;
      codeBlockFence = codeBlockStartMatch[1].trim();
      currentTemplateData.firstCodeBlockCaptured = true;
      continue;
    }

    if (!inCodeBlock && currentTemplateData && !currentTemplateData.firstCodeBlockCaptured) {
      currentTemplateData.rawDescriptionLines.push(line);
    }
  }

  finalizeCurrentTemplate();
  if (currentCategory && currentCategory.categoryName.trim() && currentCategory.templates.length > 0) {
    result.push(currentCategory);
  }

  return result;
}