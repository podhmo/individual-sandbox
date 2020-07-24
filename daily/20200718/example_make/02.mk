foo := $(shell python echo.py xxx)
bar := $(shell python echo.py $(foo) @ $(foo))
boo := $(shell python echo.py [ $(bar) ])

$(info $$(foo) is $(foo))
$(info $$(bar) is $(bar))
$(info $$(boo) is $(boo))

default:
