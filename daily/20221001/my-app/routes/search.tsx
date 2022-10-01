import { Handlers, PageProps } from "$fresh/server.ts";

const NAMES = ["Alice", "Bob", "Charile", "Dave", "Eve", "Frank"];

interface Data {
    results: string[];
    query: string;
}

export const handler: Handlers<Data> = {
    GET(req, ctx) {
        const url = new URL(req.url);
        const query = url.searchParams.get("q") || "";
        const results = NAMES.filter((name) => name.includes(query))
        return ctx.render({ results, query })
    },
};

export default function Page({ data }: PageProps<Data>) {
    const { results, query } = data;
    return (
        <div>
            <form>
                <input type="text" name="q" value={query}></input>
                <button type="submit">Search</button>
            </form>
            <ul>
                {results.map((name: string) => <li key={name}>{name}</li>)}
            </ul>
        </div>
    );
}