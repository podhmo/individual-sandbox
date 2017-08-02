default: 00 01 02

00:
	echo '{"ok": true}' | kamidana -i json 00hello.j2 > 00ok.txt
	echo '{"ok": false}' | kamidana -i json 00hello.j2 > 00ng.txt

01:
	echo '{"ok": true, "name": "foo", "subject": "world", "default": "bye"}' | kamidana -i json 01hello.j2 > 01ok.txt
	echo '{"ok": false, "name": "foo", "subject": "world", "default": "bye"}' | kamidana -i json 01hello.j2 > 01ng.txt

02:
	echo '{"ok": true, "name": "foo", "subject": "world", "default": "bye"}' | kamidana -a a.py -i json 02hello.j2 > 02ok.txt
	echo '{"ok": false, "name": "foo", "subject": "world", "default": "bye"}' | kamidana -a a.py -i json 02hello.j2 > 02ng.txt
