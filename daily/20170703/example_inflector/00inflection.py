import inflection as i

text = "foo_bar_boo"
print("camelize", i.camelize(text))

print("dasherize", i.dasherize(text))

print("humanize", i.humanize(text))

print("----------------------------------------")

print("pluralize", i.pluralize("dog"))

print("singularize", i.singularize("dogs"))


print(i.camelize(i.singularize("cats")))
