00:
	jqfpy 'from dictknife import DictWalker; w = DictWalker(["paths","operationId"]); {d["operationId"]: f"""{path[2].upper()}\t{path[1]}\t{d.get("summary") or d.get("description", "")}""" for path, d in w.walk(get())}' petstore.yaml

01:
	jqfpy 'from dictknife import DictWalker; w = DictWalker(["paths","operationId"]); [{"id": d["operationId"], "method": path[2], "path": path[1], "summary": d.get("summary") or d.get("description") or ""} for path, d in w.walk(get())]' petstore.yaml --squash -c

