> 個人的にはmock無しに実行した時にはurlopenを使った処理ですらhttp requestせずエラーになってほしい気持ちはある。

- urllibにはinstall_openerを付けるのが良さそう？
- requestsでも同じ方法で対応できる？ -> 無理そう
- 雑に[requests_mock](https://github.com/openstack/requests-mock)を使うのが良いかもしれない

[HTTPretty](https://github.com/gabrielfalcao/)や[responses](https://github.com/getsentry/responses)というライブラリを見つけたがこちらを使うと透過的に管理できるんだろうか？
