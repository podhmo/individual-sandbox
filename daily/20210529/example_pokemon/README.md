## データ

```console
echo '.schema data' |  sqlite3 pokemon.db
CREATE TABLE data (
	"index" BIGINT, 
	"図鑑番号" BIGINT, 
	"ポケモン名" TEXT, 
	"タイプ１" TEXT, 
	"タイプ２" TEXT, 
	"通常特性１" TEXT, 
	"通常特性２" TEXT, 
	"夢特性" TEXT, 
	"HP" BIGINT, 
	"こうげき" BIGINT, 
	"ぼうぎょ" BIGINT, 
	"とくこう" BIGINT, 
	"とくぼう" BIGINT, 
	"すばやさ" BIGINT, 
	"合計" BIGINT
);
CREATE INDEX ix_data_index ON data ("index");
```

## dbの生成

```console
$ pip install sqlalchemy pandas
$ make pokemon.db
```
