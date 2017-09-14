const semver = require("semver");

function check(cands, version, loose) {
  console.log("%s: %s", version, semver.maxSatisfying(cands, version, loose))
}

var cands = ["1.1.1-a.1", "1.1.1-a.11", "1.1.1-a.111", "1.1.1-a.21"];

check(cands, "~1.1.1");
check(cands, "~1.1.1", true);
check(cands, "~1.1.1-");
check(cands, "~1.1.1-", true);
check(cands, "~1.1.1-*");
check(cands, "~1.1.1-*", true);
