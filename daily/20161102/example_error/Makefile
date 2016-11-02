default:
	rm -f readme.md
	echo '# wrap many\n' >> readme.md
	echo '```bash\ngo run 00*/*' >> readme.md
	go run 00*/*   | sed "s@`pwd`@.@" >> readme.md
	echo '```\n' >> readme.md
	echo '# wrap once\n' >> readme.md
	echo '```bash\ngo run 01*/*' >> readme.md
	go run 01*/*   | sed "s@`pwd`@.@" >> readme.md
	echo '```\n' >> readme.md
