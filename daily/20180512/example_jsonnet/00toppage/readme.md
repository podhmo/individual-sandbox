```console
== input example1.jsonnet ==
// Edit me!
{
  person1: {
    name: "Alice",
    welcome: "Hellto " + self.name + "!",
  },
  person2: self.person1 { name: "Bob" },
}

== output example1.jsonnet ==
jsonnet example1.jsonnet | tee output/example1.json
{
   "person1": {
      "name": "Alice",
      "welcome": "Hellto Alice!"
   },
   "person2": {
      "name": "Bob",
      "welcome": "Hellto Bob!"
   }
}


== input example2.jsonnet ==
// A function that returns an object.
local Person(name='Alice') = {
  name: name,
  welcome: 'Hello ' + name + '!',
};
{
  person1: Person(),
  person2: Person('Bob'),
}
== output example2.jsonnet ==
jsonnet example2.jsonnet | tee output/example2.json
{
   "person1": {
      "name": "Alice",
      "welcome": "Hello Alice!"
   },
   "person2": {
      "name": "Bob",
      "welcome": "Hello Bob!"
   }
}
```
