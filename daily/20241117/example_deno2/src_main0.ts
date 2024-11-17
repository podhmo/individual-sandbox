import {chunk} from "jsr:@std/collections@1.0.0/chunk";

const arr = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
for (const chunked of chunk(arr, 3)) {
    console.log(chunked);
}