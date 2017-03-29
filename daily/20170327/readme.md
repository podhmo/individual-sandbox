# memo 名前 *-case

- camel case -- `fooBarBoo`
- snake case -- `foo_bar_boo`
- kebab case, lisp case -- `foo-bar-boo`

## node js kebab-caseにしたい


これが便利。 https://github.com/domchristie/humps

```js
humps.decamelize('helloWorldFooBar') // 'hello_world_foo_bar'
humps.decamelize('helloWorldFooBar', { separator: '-' }) // 'hello-world-foo-bar'
```

