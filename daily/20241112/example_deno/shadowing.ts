const ob = { name: "John", age: 25 };
{
    // [deno-ts] 'ob' implicitly has type 'any' because it does not have a type annotation and is referenced directly or indirectly in its own initializer.
    const ob = { ...ob, nickname: "J" };
}
