const p = new Promise((resolve, reject) => {
  console.log("start")
  setTimeout(() => {
    console.log("end");
    resolve("ok");
  }, 300);
})

p.then((x) => { console.log(`use ${x}`) });
p.then((x) => { console.log(`use2 ${x}`) });
