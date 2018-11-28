## python pydantic

- BaseModelがちょっときもい
- `**data` とかするのか
- defaultの値を関数で決めたい
- required/unrequiredの方法は？
- optional(None)の方法は？
- 全体にかけるvalidationの方法は？
- Dictのままであって欲しい場合や他のmodelにmappingされて欲しい場合は？

### 追記

- optionalの対応をするのはNoneをクラス変数に代入しておくことっぽい？
- datetime.nowをdefaultにするのはvalidatorにalways=Trueをつけてreturn value

  - (validatorはデフォルトでは値が与えられていない時に呼ばれない)

- collection自体にvalidationかけたい場合にwhole=Trueをつける

  - (collectionはSet,List,Dict)

### 追記

- primitive valueだけのschemaはどうやる？
- additionalProperties
- patternProperties
- allOf
- oneOf
- anyOf
- Dictで返す場合にはどうすれば良いんだろう？

## arch unity

https://wiki.archlinux.jp/index.php/Unity3D

```console
$ yay --editmenu --nodiffmenu  -S unity-editor
```

unityID 大文字いつもの１
