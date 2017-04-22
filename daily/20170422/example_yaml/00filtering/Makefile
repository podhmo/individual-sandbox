default:
	zenmai -m ./fns.py main.yaml

readme:
	echo '```bash' > readme.md
	echo '$ zenmai -m ./fns.py main.yaml' >> readme.md
	zenmai -m ./fns.py main.yaml >> readme.md
	echo '```' >> readme.md
	rm -rf __pycache__
