type Direction = "up" | "down" | "left" | "right";

function use(d: Direction){
  const d = "u";
  if (d === "UP") { // not "up"
    console.log("UUUUUUUUUUUUUUUUUUUUUUUUPPPPPPP");
  } else {
    console.log("ELSE");
  }
}