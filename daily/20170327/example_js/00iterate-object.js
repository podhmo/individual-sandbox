const ob = {x: 1};


for (const k in ob) {
  if (ob.hasOwnProperty(k)) {
    console.log(k);
  }
}
// console.log(ob.getOwnPropertyNames());

console.log(Object.entries(ob));
