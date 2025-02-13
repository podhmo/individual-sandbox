// ChatGPTの会話をMarkdown形式でクリップボードにコピーするスクリプト
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


