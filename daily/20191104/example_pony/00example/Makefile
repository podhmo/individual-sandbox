N ?=
DB := estore.sqlite

connect: ${DB}
	 sqlite3 ${DB}
select: ${DB}
	printf '.tables\nselect * from Customer;' | sqlite3 -echo -csv -header ${DB}
dump: ${DB}
	echo '.dump' | sqlite3 ${DB} | tee dump${N}.sql
	echo '.schema' | sqlite3 ${DB} | tee schema${N}.sql

${DB}:
	cat dump${n}.sql | sqlite3 ${DB}
