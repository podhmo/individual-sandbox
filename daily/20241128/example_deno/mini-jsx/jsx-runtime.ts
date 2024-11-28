interface Node {
    tag: string
    props: Record<string, unknown> & { children: Node[] }
    key: string | undefined
}

export function jsx(
    tag: string,
    props: Record<string, unknown> & { children: Node[] },
    key?: string,
): Node {
    return { tag, props, key }
}

export function jsxs(
    tag: string,
    props: Record<string, unknown> & { children: Node[] },
    key?: string,
): Node {
    return jsx(tag, props, key) // simplified for now
}

export function Fragment(props: Record<string, unknown> & { children: Node[] }) {
    return props.children
}
