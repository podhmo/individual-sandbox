default:
	for i in `find . -name readme.md`; do cat $$i | grep -P '^#[^#]'; done
