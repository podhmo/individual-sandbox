const text = `{
    "hello": "world",
    "numbers": [1, 2, 3]
  }`;
const data = JSON.parse(text);
console.log(data.hello);
console.log(data.numbers.length);

const obj = {
    hello: "world",
    numbers: [1, 2, 3],
};

{
    const json = JSON.stringify(obj);
    console.log(json);
    // {"hello":"world","numbers":[1,2,3]}

}

{
    const json = JSON.stringify(obj, null, 2);
    console.log(json);
}
