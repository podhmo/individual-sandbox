// JSX namespace
export interface Node {
   // deno-lint-ignore ban-types
   tag: string | Function
   props: Record<string, unknown> & { children: Node[] }
   key: string | undefined
}


export type IntrinsicElements = {
   // [P : string]: {className?: string, children?: Node[]}

   section: {className?: string, children?: Node[]}
   h1: {className?: string, children?: Node[]}
}
