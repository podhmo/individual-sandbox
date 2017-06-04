default:
	cat apps.json | python -c 'print("\n".join(sorted(k for k, v in __import__("json").load(__import__("sys").stdin)["apps"].items() if v["use"])))'

