type Direction = "u" | "d" | "r" | "l";

function main(){
  const d = "u";
  if (d === "U") { // not "u"
    console.log("U");
  } else {
    console.log("other");
  }
}