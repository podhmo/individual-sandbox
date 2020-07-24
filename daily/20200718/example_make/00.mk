foo = xxx
bar = $(foo) @ $(foo)
boo = [ $(bar) ]

# or
# foo := xxx
# bar := $(foo) @ $(foo)
# boo := [ $(bar) ]

$(info * $$(foo) is $(foo))
$(info * $$(bar) is $(bar))
$(info * $$(boo) is $(boo))

default:
