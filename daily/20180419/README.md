## golang twitter 久しぶりにgo関連の人のリストを作ってみる

- connpassあたりからtwitterのprofileを集める
- goqueryを使ってみる

[やった](./example_qoquery)

### twitterのAPIの使いかた忘れてしまっている

- [GET lists/list](https://developer.twitter.com/en/docs/accounts-and-users/create-manage-lists/api-reference/get-lists-list)
- [POST lists/create](https://developer.twitter.com/en/docs/accounts-and-users/create-manage-lists/api-reference/post-lists-create)
- [POST lists/members/create_all](https://developer.twitter.com/en/docs/accounts-and-users/create-manage-lists/api-reference/post-lists-members-create_all)

:thought_balloo: screen_nameで良いので別にuser_idを調べる必要はなかった

```
GET https://api.twitter.com/1.1/lists/list.json?screen_name=twitterapi
POST https://api.twitter.com/1.1/lists/create.json?name=Goonies&mode=public&description=For%20life
POST https://api.twitter.com/1.1/lists/members/create_all.json?screen_name=rsarver,episod,jasoncosta,theseancook,kurrik,froginthevalley&list_id=23
```
