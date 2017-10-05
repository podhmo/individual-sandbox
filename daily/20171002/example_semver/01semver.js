const semver = require("semver");
var cands = ["1.2.3-alpha.1"];
console.log(semver.maxSatisfying(cands, "~1.2.3"));
console.log(semver.maxSatisfying(cands, "~1.2.3", true));
console.log(semver.maxSatisfying(cands, "~1.2.3-"));
console.log(semver.maxSatisfying(cands, "~1.2.3-", true)); // 1.2.3-a.111 (only this)
console.log(semver.maxSatisfying(cands, "~1.2.3-*")); 
console.log(semver.maxSatisfying(cands, "~1.2.3-*", true));
