var BKDRHash = function(str) {
    var seed = 131;
    var seed2 = 137;
    var hash = 0;
    // make hash more sensitive for short string like 'a', 'b', 'c'
    str += 'x';
    // Note: Number.MAX_SAFE_INTEGER equals 9007199254740991
    var MAX_SAFE_INTEGER = parseInt(9007199254740991 / seed2);
    for(var i = 0; i < str.length; i++) {
        if(hash > MAX_SAFE_INTEGER) {
          console.log(`\t\t!${str[i]}`)
            hash = parseInt(hash / seed2) + str.charCodeAt(i);
        }
        console.log(`\t${str[i]}: ${hash} * ${seed} + ${str.charCodeAt(i)};`)
        hash = hash * seed + str.charCodeAt(i);
    }
    return hash;
};

function run(s){
  console.log(`${s} -> ${BKDRHash(s)}`)
}

run("localhost:8000")
run("localhost:7000")
// run("localhost8000")
// run("localhost7000")
// run("host:8000")
// run("host:7000")
// run("hos:8000")
// run("hos:7000")
// run("ho:8000")
// run("ho:7000")
