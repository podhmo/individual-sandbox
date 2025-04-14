import { TextLineStream } from "jsr:@std/streams@1.0.9";

const filename = "README.md";

// cat like
{
    using input = await Deno.open(filename);
    for await (
        const line of input
            .readable.pipeThrough(new TextDecoderStream())
            .pipeThrough(new TextLineStream())
    ) {
        console.log(line);
    }

    // stdout is closed automatically?
    // await input.readable.pipeTo(
    //     Deno.stdout.writable,
    // );
}

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

// cat like with line number and pipe
{
    let i = 1;
    const pipe = new TransformStream({
        transform(chunk, controller) {
            controller.enqueue(`${i++}: ${chunk}`);
        },
    });

    using input = await Deno.open(filename);
    for await (
        const line of input
            .readable.pipeThrough(new TextDecoderStream())
            .pipeThrough(new TextLineStream())
            .pipeThrough(pipe)
    ) {
        console.log(line);
    }
}
