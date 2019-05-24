OPTS ?= -echo -column -header

demo: setup
	$(MAKE) -s _demo
_demo: inner-join left-outer-join full-outer-join

inner-join:
	$(info ****************************************)
	$(info $@)
	$(info ****************************************)
	echo 'select * from users as u join groups as g on u.gid = g.gid' | sqlite3 ${OPTS} data.db
	echo 'select * from groups as g join users as u on u.gid = g.gid' | sqlite3 ${OPTS} data.db

left-outer-join:
	$(info ****************************************)
	$(info $@)
	$(info ****************************************)
	echo 'select * from users as u left outer join groups as g on u.gid = g.gid' | sqlite3 ${OPTS} data.db
	echo 'select * from groups as g left outer join users as u on u.gid = g.gid' | sqlite3 ${OPTS} data.db

full-outer-join:
	$(info ****************************************)
	$(info $@)
	$(info ****************************************)
	echo 'select * from users as u left outer join groups as g on u.gid = g.gid union select * from groups as g left outer join users as u on g.gid = u.gid where u.gid is null' | sqlite3 ${OPTS} data.db

setup: clean
	cat schema.sql | sqlite3 ${OPTS} data.db
	cat data.sql | sqlite3 ${OPTS} data.db
clean:
	rm -rf *.db
