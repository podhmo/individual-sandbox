N ?=
DB := estore.sqlite

connect: ${DB}
	 sqlite3 ${DB}
select: ${DB}
	printf '.headers on\n.mode csv\n.tables\nselect * from Customer;' | sqlite3 ${DB}
dump: ${DB}
	echo '.dump' | sqlite3 ${DB} | tee dump${N}.sql

${DB}:
	cat dump${n}.sql | sqlite3 ${DB}
