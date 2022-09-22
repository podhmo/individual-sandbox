import React from "https://esm.sh/react@18.2.0";
import ReactDomServer from "https://esm.sh/react-dom@18.2.0/server";
import ReactMarkdown from "https://esm.sh/react-markdown@8.0.3"

// https://github.com/remarkjs/react-markdown

const text = `
# Heading (rank 1)
## Heading 2
### 3
#### 4
##### 5
###### 6

> Block quote

* Unordered
* List

1. Ordered
2. List

A paragraph, introducing a thematic break:

---

a [link](https://example.com), an ![image](./image.png), some *emphasis*,
something **strong**, and finally a little code().
`;

const s = ReactDomServer.renderToString(<ReactMarkdown>{text}</ReactMarkdown>);
console.log(s);
