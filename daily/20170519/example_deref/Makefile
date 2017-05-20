all: 00 01 02 03 04 06 10

00:
	jsonknife deref --src definitions.yaml > dst/00.yaml
01:
	jsonknife deref --src definitions.yaml --ref="#/definitions/person" > dst/01.yaml
02:
	jsonknife deref --src definitions.yaml --ref="#/definitions/person" | dictknife transform --code 'lambda d: {"definitions": {"me": d}}' > dst/02.yaml
03:
	jsonknife deref --src definitions.yaml --ref="#/definitions/person" --unwrap="#/definition/me" > dst/03.yaml
04:
	jsonknife deref --src definitions.yaml --ref="#/definitions/name" --ref="#/definitions/age" > dst/04.yaml
# 05:
# 	jsonknife deref --src definitions.yaml --ref="#/definitions/name" --ref="#/definitions/age" --with-name > dst/05.yaml
06:
	jsonknife deref --src definitions.yaml --ref="#/definitions/name@#/definitions/NAME" --ref="#/definitions/age@#/definitions/AGE" > dst/06.yaml

10:
	jsonknife deref --src paths.yaml --ref="#/paths/~1hello~1name/get" > dst/11.yaml
