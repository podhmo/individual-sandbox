const foo = {
    name: "foo",
    age: 10,
    sayHello: function () {
        console.log(`Hello, I'm ${this.name}`);
    },
    [Symbol("help")]: "hoi",
};

{
    const ob = foo; // [deno-ts] Type '{ [x: symbol]: string; name: string; age: number; sayHello: () => void; }'
    console.log("json");
    console.log(JSON.stringify(ob)); // {"name":"foo","age":10}
    console.log("----------------------------------------");
    console.log("console.dir");
    console.dir(ob, { depth: null }); // { name: 'foo', age: 10, sayHello: [Function (anonymous)], [Symbol(help)]: 'hoi' }
}

console.log("");
console.log("");

{
    const ob = { ...foo };
    console.log("json");
    console.log(JSON.stringify(ob)); // {"name":"foo","age":10}
    console.log("----------------------------------------");
    console.log("console.dir");
    console.dir(ob, { depth: null }); // { name: 'foo', age: 10, sayHello: [Function (anonymous)], [Symbol(help)]: 'hoi' }
}
