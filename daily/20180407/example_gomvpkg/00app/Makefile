PKG = github.com/xxx/myapp
PKGDIR = $(GOPATH)/src/$(PKG)

default:
	cd $(GOPATH)/src/github.com/podhmo/gomvpkg-light; 	go install -v
	cd $(PKGDIR); 	git reset --hard
	# gomvpkg-light --in $(PKG) --from $(PKG)/testdata --to $(PKG)/test/fixture --only
	gomvpkg --from $(PKG)/testdata --to $(PKG)/test/fixture

light:
	cd $(GOPATH)/src/github.com/podhmo/gomvpkg-light; 	go install -v
	cd $(PKGDIR); 	git reset --hard
	gomvpkg-light --in $(PKG) --from $(PKG)/model --to $(PKG)/model2

original:
	cd $(PKGDIR); 	git reset --hard
	gomvpkg -from $(PKG)/model -to $(PKG)/model2
	# gomvpkg-light --in $(PKG) --from $(PKG)/googleapi --to $(PKG)/google
