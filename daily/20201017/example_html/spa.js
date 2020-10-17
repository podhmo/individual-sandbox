const EXTRACT = /\/:[^/]+/g;
const OPTIONAL_SYMBOL = /\?$/;
const OPTIONAL_MATCHER_STRING = "(?:/([^/]+?))?";
const REQUIRED_MATCHER_STRING = "/([^/]+?)";

const matcherToParamResolver = (matcher) => {
  const extracted = matcher.match(EXTRACT);
  const keys =
    extracted &&
    extracted
      .map((e) => e.replace(OPTIONAL_SYMBOL, ""))
      .map((e) => e.replace(/^\/:/, ""));
  const exp = matcher.replace(EXTRACT, (e) =>
    OPTIONAL_SYMBOL.test(e) ? OPTIONAL_MATCHER_STRING : REQUIRED_MATCHER_STRING
  );
  const reg = new RegExp(`^${exp}(?:/)?$`);
  return (path) => {
    const res = reg.exec(path);
    if (res) {
      if (keys) {
        const params = {};
        res.slice(1).forEach((e, i) => {
          params[keys[i]] = e;
        });
        return params;
      }
      return {};
    } else {
      return null;
    }
  };
};

const router = (
  pages,
  notfound,
  container
) => {
  //引数で受け取ったpagesをビルドする。
  const builtPages = Object.keys(pages).map((matcher) => ({
    render(params) {
      const page = pages[matcher];
      return typeof page === "string" ? page : page(params);
    },
    test: matcherToParamResolver(matcher),
  }));
  //DOM書き換え処理
  const updateView = () => {
    //innerHTMLを使ってDOMを書き換える
    const mount = (html) => {
      container.innerHTML = html;
    };
    const path = window.location.pathname;
    //マッチャーにマッチするページまでforで繰り返し
    for (const page of builtPages) {
      const params = page.test(path);
      if (params) {
        mount(page.render(params));
        //見つかればreturn
        return;
      }
    }
    //見つからなければ404のページを表示
    mount(notfound);
  };

  //アンカーリンクにルーターのリンクを仕込む
  document.querySelectorAll("a").forEach((a) => {
    a.onclick = (event) => {
      event.preventDefault();
      window.history.pushState(null, "", a.href);
      updateView();
    };
  });

  //ブラウザバックを監視
  window.addEventListener("popstate", () => {
    updateView();
  });

  //初期化
  updateView();
};

const pages = {
  "/": `
    <h1>Vanilla SPA</h1>
  `,
  "/home": `
    <h1>ようこそ！</h1>
  `,
  "/profile/:name": (params) => `
    <h1>私は${params.name}です。</h1>
  `,
};

const container = document.getElementById("app");
container && router(pages, `<h1>404 : Not Found<h1>`, container);
