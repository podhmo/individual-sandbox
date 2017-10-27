/*
Just draw a border round the document.body.
*/
var arr = [1, 1, 1];
Array.from(document.domain).forEach((c,  i) => {
  const k = i % arr.length;
  arr[k] *= c.charCodeAt();
});
const hexcolor = arr.map((n) => {
  const r = n % 256;
  const hex = r.toString(16);
  return hex.length <= 1 ? "0" + hex  : hex;
}).join("");
document.body.style.border = `20px solid #${hexcolor}`;
