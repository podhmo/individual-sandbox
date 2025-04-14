import {
    concatReadableStreams,
    mergeReadableStreams,
    toText,
    toTransformStream,
} from "jsr:@std/streams@1.0.9";

// manual writable stream with ReadableStream.from
{
    const redable = ReadableStream.from([1, 2, 3, 4, 5]);
    const writable = new WritableStream({
        write: (chunk) => {
            console.log(chunk);
        },
    });

    // use it
    await redable.pipeTo(writable);
}

console.log("");

// manual pipe
{
    const readable = new ReadableStream({
        pull: (controller) => {
            for (let i = 0; i < 10; i++) {
                controller.enqueue(i);
            }
            controller.close();
        },
    });

    const writable = new WritableStream({
        write: (chunk) => {
            console.log(chunk);
        },
    });

    const transform = new TransformStream({
        transform(chunk, controller) {
            controller.enqueue(chunk * 2);
        },
    });

    // use it
    await readable.pipeThrough(transform).pipeTo(writable);
}

console.log("");

// using toText, toTransformStream
{
    // concat
    {
        const stream = concatReadableStreams(
            ReadableStream.from([1, 2, 3, 4, 5]),
            ReadableStream.from([10, 20, 30, 40, 50]),
        ).pipeThrough(toTransformStream(
            async function* (src) {
                for await (const chunk of src) {
                    yield `${chunk * 2}, `;
                }
            },
        ));

        // use it
        console.log("%o", await toText(stream));
    }

    // merge
    {
        const stream = mergeReadableStreams(
            ReadableStream.from([1, 2, 3, 4, 5]),
            ReadableStream.from([10, 20, 30, 40, 50]),
        ).pipeThrough(toTransformStream(
            async function* (src) {
                for await (const chunk of src) {
                    yield `${chunk * 2}, `;
                }
            },
        ));

        // use it
        console.log("%o", await toText(stream));
    }
}
