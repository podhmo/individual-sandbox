run:
	python -m main.py

build:
	python -m zipapp -c -o app.pyz ../myzipapp -m "main:main"

clean:
	rm -f app.pyz
	find . -name __pycache__ -type d -exec rm -rf {} + 

.PHONY: run build clean
