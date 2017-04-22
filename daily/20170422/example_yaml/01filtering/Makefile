default:
	zenmai main.yaml

readme:
	echo '```bash' > readme.md
	echo '$ zenmai main.yaml' >> readme.md
	zenmai  main.yaml >> readme.md
	echo '```' >> readme.md
	rm -rf __pycache__
