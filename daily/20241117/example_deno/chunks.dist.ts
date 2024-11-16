// https://jsr.io/@std/collections/1.0.9/chunk.ts
function chunk(array, size) {
  if (size <= 0 || !Number.isInteger(size)) {
    throw new RangeError(
      `Expected size to be an integer greater than 0 but found ${size}`
    );
  }
  const result = [];
  let index = 0;
  while (index < array.length) {
    result.push(array.slice(index, index + size));
    index += size;
  }
  return result;
}

// chunks.ts
var arr = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
for (const c of chunk(arr, 3)) {
  console.log(c);
}
