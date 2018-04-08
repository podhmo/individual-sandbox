PKG = github.com/xxxx/myapp
PKGDIR = $(GOPATH)/src/$(PKG)

light:
	cd $(GOPATH)/src/github.com/podhmo/gomvpkg-light; 	go install -v
	cd $(PKGDIR); 	git reset --hard
	gomvpkg-light --in $(PKG) --from $(PKG)/model --to $(PKG)/model2 --disable-gc

light-profile:
	cd $(GOPATH)/src/github.com/podhmo/gomvpkg-light; 	go install -v
	cd $(PKGDIR); 	git reset --hard
	gomvpkg-light --in $(PKG) --from $(PKG)/model --to $(PKG)/model2 --profile a.prof

original:
	cd $(PKGDIR); 	git reset --hard
	gomvpkg -from $(PKG)/model -to $(PKG)/model2
	# gomvpkg-light --in $(PKG) --from $(PKG)/googleapi --to $(PKG)/google
