let i = 0;
function delayed(x, n) {
  i++
  let j= i;
  console.log(`  start: ${j} (delayed=${n})`);
  return new Promise((resolve, reject) => {
    setTimeout(function(){
      console.log(`  end: ${j}`);
      resolve(x);
    },  n);
  });
}

delayed(10, 100).then((x) => {
  console.log(`hoi: ${x}`);
  return x;
}).then((x) =>{
  return delayed(x, 200);
}).then((x) => {
  console.log(`hoi: ${x}`);
  return x;
});

Promise.race([delayed(10, 100), delayed(11, 100), delayed(12, 200)]).then((x) => {
  console.log(`ahyaya: ${x}`);
});
