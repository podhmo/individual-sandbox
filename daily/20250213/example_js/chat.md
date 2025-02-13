### あなた:

chatgptでのweb UIでのやり取りをmarkdown形式でクリップボードにコピーするjsのコードがほしいです。devtoolsから実行します

### ChatGPT:

ChatGPTのWeb UI上でのやり取りをMarkdown形式でクリップボードにコピーするためのJavaScriptコードを、ブラウザの開発者ツール（DevTools）から実行する方法をご紹介します。以下のコードをDevToolsのコンソールに貼り付けて実行してください。

```コードをコピーする
(function() {
  if (!document.URL.startsWith('https://chat.openai.com')) {
    alert('このスクリプトは https://chat.openai.com 上で実行してください。');
    return;
  }

  const sanitize = html => {
    return String(html).replace(/&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;')
      .replace(/"/g, '&quot;');
  };

  const tableToMarkdown = element => {
    const rows = [...element.rows].map(row => [...row.cells].map(cell => cell.innerText));
    const header = rows.shift();
    const separator = header.map(() => '---');
    const body = rows.map(row => `| ${row.join(' | ')} |`);
    return `| ${header.join(' | ')} |\n|${separator.join('|')}|\n${body.join('\n')}`;
  };

  const escapeTags = text => {
    return String(text).replace(/<(\/?[a-zA-Z]+)>/g, '`<$1>`');
  };

  const contentToMarkdown = element => {
    const tag = element.tagName;
    if (tag === 'OL') return [...element.querySelectorAll('li')].map((li, i) => `${i + 1}. ${li.innerText}`).join('\n');
    if (tag === 'UL') return [...element.querySelectorAll('li')].map(li => `- ${li.innerText}`).join('\n');
    if (tag === 'PRE') {
      const lang = element.querySelector('span').innerText;
      return '```' + lang + '\n' + element.querySelector('code').innerText + '\n```';
    }
    if (tag === 'TABLE') return tableToMarkdown(element);
    return escapeTags(element.innerText);
  };

  const messages = [...document.querySelectorAll('div[data-message-author-role]')];
  const markdownText = messages.map(m => {
    const author = m.getAttribute('data-message-author-role') === 'user' ? 'あなた' : 'ChatGPT';
    const elements = m.querySelectorAll('p, ol, ul, pre, table');
    const content = elements.length === 0 ? m.innerText : [...elements].map(e => contentToMarkdown(e)).join('\n\n');
    return `## ${author}:\n\n${content}`;
  }).join('\n\n');

  navigator.clipboard.writeText(markdownText).then(() => {
    alert('ChatGPTとのやり取りがMarkdown形式でクリップボードにコピーされました。');
  }).catch(err => {
    console.error('クリップボードへのコピーに失敗しました: ', err);
  });
})();

```

使用手順:

1. ChatGPTのWebページ（https://chat.openai.com）を開きます。
2. ブラウザの開発者ツール（DevTools）を開きます。
Windows/Linux: Ctrl + Shift + I
Mac: Command + Option + I
3. Windows/Linux: Ctrl + Shift + I
4. Mac: Command + Option + I
5. 「コンソール」タブを選択します。
6. 上記のコードをコピーし、コンソールに貼り付けてEnterキーを押して実行します。

- Windows/Linux: Ctrl + Shift + I
- Mac: Command + Option + I

実行後、ChatGPTとのやり取りがMarkdown形式でクリップボードにコピーされ、「ChatGPTとのやり取りがMarkdown形式でクリップボードにコピーされました。」というメッセージが表示されます。その後、お好みのエディタやドキュメントに貼り付けてご利用ください。

このコードは、ChatGPTとの会話をMarkdown形式に変換し、クリップボードにコピーするものです。元のコードはQiitaの記事「ChatGPTとの会話をMarkdownに変換するブックマークレット」で紹介されています。 
QIITA.COM

なお、ブラウザのセキュリティ設定やバージョンによっては、クリップボードへのアクセスが制限されている場合があります。その場合、コード内のnavigator.clipboard.writeTextの部分を別の方法に置き換える必要があるかもしれません。

### あなた:

ブックマークレットにしてください


### ChatGPT:

ブックマークレットとして使えるように、以下の形式に変換しました。ブラウザのブックマークバーに追加し、ChatGPTのWebページ上でクリックすると、会話をMarkdown形式でクリップボードにコピーできます。

```Copy code
javascript:(function(){if(!document.URL.startsWith('https://chat.openai.com')){alert('このスクリプトは https://chat.openai.com 上で実行してください。');return;}const sanitize=html=>String(html).replace(/&/g,'&amp;').replace(/</g,'&lt;').replace(/>/g,'&gt;').replace(/"/g,'&quot;');const tableToMarkdown=element=>{const rows=[...element.rows].map(row=>[...row.cells].map(cell=>cell.innerText));const header=rows.shift();const separator=header.map(()=> '---');const body=rows.map(row=> `| ${row.join(' | ')} |`);return `| ${header.join(' | ')} |\n|${separator.join('|')}|\n${body.join('\n')}`;};const escapeTags=text=>String(text).replace(/<(\/?[a-zA-Z]+)>/g,'`<$1>`');const contentToMarkdown=element=>{const tag=element.tagName;if(tag==='OL')return [...element.querySelectorAll('li')].map((li,i)=> `${i+1}. ${li.innerText}`).join('\n');if(tag==='UL')return [...element.querySelectorAll('li')].map(li=> `- ${li.innerText}`).join('\n');if(tag==='PRE'){const lang=element.querySelector('span').innerText;return '```'+lang+'\n'+element.querySelector('code').innerText+'\n```';}if(tag==='TABLE')return tableToMarkdown(element);return escapeTags(element.innerText);};const messages=[...document.querySelectorAll('div[data-message-author-role]')];const markdownText=messages.map(m=>{const author=m.getAttribute('data-message-author-role')==='user'?'あなた':'ChatGPT';const elements=m.querySelectorAll('p, ol, ul, pre, table');const content=elements.length===0?m.innerText:[...elements].map(e=>contentToMarkdown(e)).join('\n\n');return `## ${author}:\n\n${content}`;}).join('\n\n');navigator.clipboard.writeText(markdownText).then(()=>alert('ChatGPTとのやり取りがMarkdown形式でクリップボードにコピーされました。')).catch(err=>console.error('クリップボードへのコピーに失敗しました: ',err));})();

```

1. ブックマークを作成

お使いのブラウザの「ブックマーク」メニューから「新しいブックマークを追加」を選択。
名前: ChatGPT Markdownコピー （任意）
URL: 上記の javascript:(function(){...})(); のコードをコピーして貼り付ける。
2. お使いのブラウザの「ブックマーク」メニューから「新しいブックマークを追加」を選択。
3. 名前: ChatGPT Markdownコピー （任意）
4. URL: 上記の javascript:(function(){...})(); のコードをコピーして貼り付ける。
5. ChatGPTのページ (https://chat.openai.com) を開く
6. 作成したブックマークをクリック

実行すると、会話がMarkdown形式でクリップボードにコピーされます。
7. 実行すると、会話がMarkdown形式でクリップボードにコピーされます。

ブックマークを作成

- お使いのブラウザの「ブックマーク」メニューから「新しいブックマークを追加」を選択。
- 名前: ChatGPT Markdownコピー （任意）
- URL: 上記の javascript:(function(){...})(); のコードをコピーして貼り付ける。

ChatGPTのページ (https://chat.openai.com) を開く

作成したブックマークをクリック

- 実行すると、会話がMarkdown形式でクリップボードにコピーされます。

あとはエディタやドキュメントに貼り付けて利用できます！

### あなた:

devtoolsで実行できれば十分です。ただし以下のように仕様を変更してください。ユーザーの入力とgptのレスポンスを全て最初から最後までコピーするようにしてください。

### ChatGPT:

以下のJavaScriptコードをブラウザの開発者ツール（DevTools）のコンソールに貼り付けて実行すると、ChatGPTの全てのユーザー入力とレスポンスをMarkdown形式でクリップボードにコピーできます。

```Copy code
(function() {
  if (!document.URL.startsWith('https://chat.openai.com')) {
    alert('このスクリプトは https://chat.openai.com 上で実行してください。');
    return

```

### あなた:

あ普通のjsにしてください。ブックマークレット化しなくて良いです。devtoolsから実行します。

### ChatGPT:

了解しました。以下のJavaScriptコードを開発者ツール（DevTools）のコンソールに貼り付けて実行すると、ChatGPTの全てのユーザー入力とレスポンスをMarkdown形式でクリップボードにコピーできます。

```Copy code
// ChatGPTの会話をMarkdown形式でクリップボードにコピーするスクリプト
if (!document.URL.startsWith('https://chat.openai.com')) {
  console.error('このスクリプトは https://chat.openai.com 上で実行してください。');
} else {
  function tableToMarkdown(table) {
    const rows = [...table.rows].map(row => [...row.cells].map(cell => cell.innerText.trim()));
    const header = rows.shift();
    const separator = header.map(() => '---');
    const body = rows.map(row => `| ${row.join(' | ')} |`).join('\n');
    return `| ${header.join(' | ')} |\n|${separator.join('|')}|\n${body}`;
  }

  function contentToMarkdown(element) {
    if (element.tagName === 'OL') {
      return [...element.querySelectorAll('li')].map((li, i) => `${i + 1}. ${li.innerText}`).join('\n');
    }
    if (element.tagName === 'UL') {
      return [...element.querySelectorAll('li')].map(li => `- ${li.innerText}`).join('\n');
    }
    if (element.tagName === 'PRE') {
      const codeBlock = element.querySelector('code');
      const langElement = element.querySelector('span');
      const lang = langElement ? langElement.innerText.trim() : '';
      return `\`\`\`${lang}\n${codeBlock.innerText}\n\`\`\``;
    }
    if (element.tagName === 'TABLE') {
      return tableToMarkdown(element);
    }
    return element.innerText;
  }

  function extractMessages() {
    return [...document.querySelectorAll('div[data-message-author-role]')].map(message => {
      const author = message.getAttribute('data-message-author-role') === 'user' ? '### あなた:' : '### ChatGPT:';
      const elements = message.querySelectorAll('p, ol, ul, pre, table');
      const content = elements.length === 0
        ? message.innerText
        : [...elements].map(el => contentToMarkdown(el)).join('\n\n');
      return `${author}\n\n${content}`;
    }).join('\n\n');
  }

  const markdownText = extractMessages();
  
  navigator.clipboard.writeText(markdownText).then(() => {
    console.log('ChatGPTとのやり取りがMarkdown形式でクリップボードにコピーされました。');
  }).catch(err => {
    console.error('クリップボードへのコピーに失敗しました:', err);
  });
}

```

1. ChatGPTのWebページ (https://chat.openai.com) を開く。
2. 開発者ツール（DevTools）を開く。
Windows/Linux: Ctrl + Shift + I
Mac: Command + Option + I
3. Windows/Linux: Ctrl + Shift + I
4. Mac: Command + Option + I
5. 「コンソール」タブを選択。
6. 上記のコードをコピーしてコンソールに貼り付け、Enter を押して実行。

- Windows/Linux: Ctrl + Shift + I
- Mac: Command + Option + I

実行後、ChatGPTの全てのユーザー入力とレスポンスがMarkdown形式でクリップボードにコピーされるので、エディタなどに貼り付けて利用してください。

### あなた:

ブラウザ拡張化してください。全部のファイルの内容を出力してください。

### ChatGPT:

ChatGPTのやり取りをMarkdown形式でコピーするためのGoogle Chrome拡張を作成します。

- 機能: ChatGPTの会話履歴をMarkdown形式でコピー
- 動作: https://chat.openai.com 上でボタンを押すと、会話を取得してクリップボードにコピー
- 構成ファイル:
manifest.json（拡張機能の定義）
popup.html（拡張機能のUI）
popup.js（UIの動作制御）
content.js（ページ内のデータ抽出処理）
styles.css（拡張機能のUIスタイル）
- manifest.json（拡張機能の定義）
- popup.html（拡張機能のUI）
- popup.js（UIの動作制御）
- content.js（ページ内のデータ抽出処理）
- styles.css（拡張機能のUIスタイル）

1. manifest.json（拡張機能の定義）
2. popup.html（拡張機能のUI）
3. popup.js（UIの動作制御）
4. content.js（ページ内のデータ抽出処理）
5. styles.css（拡張機能のUIスタイル）

拡張機能の基本情報を定義します。

```Copy code
{
  "manifest_version": 3,
  "name": "ChatGPT Markdown Copier",
  "version": "1.0",
  "description": "ChatGPTのやり取りをMarkdown形式でコピーする拡張機能。",
  "permissions": ["activeTab", "scripting", "clipboardWrite"],
  "host_permissions": ["https://chat.openai.com/*"],
  "action": {
    "default_popup": "popup.html",
    "default_icon": {
      "16": "icon.png",
      "48": "icon.png",
      "128": "icon.png"
    }
  },
  "background": {
    "service_worker": "background.js"
  }
}

```

拡張機能のポップアップUIを定義します。

```Copy code
<!DOCTYPE html>
<html lang="ja">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>ChatGPT Copier</title>
    <link rel="stylesheet" href="styles.css">
</head>
<body>
    <h2>ChatGPT Markdown Copier</h2>
    <button id="copyButton">Markdownをコピー</button>
    <p id="status"></p>
    <script src="popup.js"></script>
</body>
</html>

```

拡張機能のボタンがクリックされたときの動作を定義します。

```Copy code
document.getElementById("copyButton").addEventListener("click", async () => {
    const [tab] = await chrome.tabs.query({ active: true, currentWindow: true });

    if (tab.url.startsWith("https://chat.openai.com")) {
        chrome.scripting.executeScript({
            target: { tabId: tab.id },
            files: ["content.js"]
        });
    } else {
        document.getElementById("status").innerText = "ChatGPTのページで実行してください。";
    }
});

```

ChatGPTの会話履歴を取得し、Markdown形式でクリップボードにコピーする処理を定義します。

```Copy code
(function() {
    function tableToMarkdown(table) {
        const rows = [...table.rows].map(row => [...row.cells].map(cell => cell.innerText.trim()));
        const header = rows.shift();
        const separator = header.map(() => '---');
        const body = rows.map(row => `| ${row.join(' | ')} |`).join('\n');
        return `| ${header.join(' | ')} |\n|${separator.join('|')}|\n${body}`;
    }

    function contentToMarkdown(element) {
        if (element.tagName === 'OL') {
            return [...element.querySelectorAll('li')].map((li, i) => `${i + 1}. ${li.innerText}`).join('\n');
        }
        if (element.tagName === 'UL') {
            return [...element.querySelectorAll('li')].map(li => `- ${li.innerText}`).join('\n');
        }
        if (element.tagName === 'PRE') {
            const codeBlock = element.querySelector('code');
            const langElement = element.querySelector('span');
            const lang = langElement ? langElement.innerText.trim() : '';
            return `\`\`\`${lang}\n${codeBlock.innerText}\n\`\`\``;
        }
        if (element.tagName === 'TABLE') {
            return tableToMarkdown(element);
        }
        return element.innerText;
    }

    function extractMessages() {
        return [...document.querySelectorAll('div[data-message-author-role]')].map(message => {
            const author = message.getAttribute('data-message-author-role') === 'user' ? '### あなた:' : '### ChatGPT:';
            const elements = message.querySelectorAll('p, ol, ul, pre, table');
            const content = elements.length === 0
                ? message.innerText
                : [...elements].map(el => contentToMarkdown(el)).join('\n\n');
            return `${author}\n\n${content}`;
        }).join('\n\n');
    }

    const markdownText = extractMessages();
    
    navigator.clipboard.writeText(markdownText).then(() => {
        alert("ChatGPTとのやり取りがMarkdown形式でクリップボードにコピーされました。");
    }).catch(err => {
        console.error("クリップボードへのコピーに失敗しました:", err);
    });
})();

```

ポップアップのスタイルを定義します。

```Copy code
body {
    font-family: Arial, sans-serif;
    text-align: center;
    padding: 10px;
    width: 200px;
}

h2 {
    font-size: 16px;
}

button {
    background-color: #4CAF50;
    color: white;
    border: none;
    padding: 10px;
    cursor: pointer;
    width: 100%;
    font-size: 14px;
}

button:hover {
    background-color: #45a049;
}

p {
    font-size: 12px;
    margin-top: 10px;
}

```

今回は特にバックグラウンド処理は必要ないので、空のスクリプトを配置します。

```Copy code
// バックグラウンド処理は不要

```

アイコン用の画像を icon.png として拡張機能のフォルダに配置してください。（サイズは 128x128 ピクセル推奨）

1. 任意のフォルダに 上記のすべてのファイル を保存する。
2. Google Chrome で chrome://extensions/ にアクセスする。
3. 「デベロッパーモード」 を有効にする。
4. 「パッケージ化されていない拡張機能を読み込む」 をクリックし、保存したフォルダを選択する。
5. 拡張機能が追加されたことを確認し、ツールバーの拡張機能アイコンから ChatGPT Markdown Copier を実行する。

1. https://chat.openai.com を開く。
2. 拡張機能をクリックし、「Markdownをコピー」ボタンを押す。
3. クリップボードにコピーされた内容をエディタなどに貼り付ける。

この拡張機能を使えば、ChatGPTの全会話を簡単にMarkdown形式で保存できます！
