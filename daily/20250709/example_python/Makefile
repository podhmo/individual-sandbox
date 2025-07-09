PYTHONDEVMODE := 1  # deprecation warningsを出力

report:
	rm -f report.md
	echo '# report' > report.md
	echo '' >> report.md
	echo '```' >> report.md
	hatch test --all --show >> report.md
	echo '```' >> report.md
	echo '' >> report.md
	echo '## test' >> report.md
	echo '' >> report.md
	echo '```' >> report.md
	hatch test --all >> report.md
	echo '```' >> report.md

clean:
	rm -fr __pycache__ 


.PHONY: report clean
