## swの管理を手軽にする方法ってなんだっけ？

workbox?
これはキャッシュをいい感じに管理するものだったみたい。なるほど。


## pwa

結局スクラップにした。 https://zenn.dev/podhmo/scraps/8a63a3c0503fe0#comment-68a310fdab546e


- https://github.com/mdn/pwa-examples/tree/master/js13kpwa
- https://web.dev/learn/pwa/
- https://developers.google.com/web/fundamentals/primers/service-workers?hl=ja

### swのevent

- install -> waitUntil()
- activate
- fetch -> respondWith()

### pwa

https://developer.mozilla.org/ja/docs/Web/Progressive_web_apps/App_structure

- index.html
- style.css
- sw.js


構成するもの

- service worker (navigator.serviceWorker.register())
- (通知の許可, Notification.requestPermission())
- cache? (caches.open()

### pwaをinstall可能にする方法

- https://developer.mozilla.org/ja/docs/Web/Progressive_web_apps/Installable_PWAs

その他の話

- A2HS (add to home screen)

### web manifest

- https://developer.mozilla.org/ja/docs/Web/Progressive_web_apps/Add_to_home_screen#manifest


## go return nilを[]に

- jsonのときのやつら
- reflectを上手く使えば対応できそう。
