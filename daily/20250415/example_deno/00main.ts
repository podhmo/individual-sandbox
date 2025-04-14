import { TextLineStream } from "jsr:@std/streams@1.0.9";

const filename = "README.md";

// cat like
{
    using input = await Deno.open(filename);
    await input.readable
        .pipeTo(Deno.stdout.writable, {
            preventClose: true,
        });
}

console.log("\n\n");

// cat like with line number
{
    using input = await Deno.open(filename);
    let i = 1;
    for await (
        const line of input
            .readable.pipeThrough(new TextDecoderStream())
            .pipeThrough(new TextLineStream())
    ) {
        console.log(`${i++}: ${line}`);
    }
}

console.log("\n\n");

// cat like with line number and pipe
{
    let i = 1;
    const pipe = new TransformStream({
        transform(chunk, controller) {
            controller.enqueue(`${i++}: ${chunk}\n`);
        },
    });

    using input = await Deno.open(filename);
    await input.readable
        .pipeThrough(new TextDecoderStream())
        .pipeThrough(new TextLineStream())
        .pipeThrough(pipe)
        .pipeThrough(new TextEncoderStream())
        .pipeTo(Deno.stdout.writable, {
            preventClose: true,
        });
}
