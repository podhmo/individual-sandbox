# tar -zcvf archive-name.tar.gz source-directory-name
# tar -cvf - file1 file2 dir3 | gzip > archive.tar.gz

00:
	docker run --rm elasticsearch:7.14.1 tar -czf - /usr/share/elasticsearch/config > config.tar.gz
	tar -tf config.tar.gz
