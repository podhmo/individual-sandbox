declare global {
    namespace JSX {
        interface IntrinsicElements {
            [elemName: string]: unknown;
        }
    }
}

export interface Node {
    tag: string;
    props?: Record<string, string>;
    children: (Node | string)[];
}

export function E(tag: string, props: Record<string, string>, ...children: Node[]): Node {
    return { tag, props, children };
}

// TODO: sanitize props
export function render(node: Node | string): string {
    if (typeof node === 'string') {
        return node;
    }

    if (!node.children) {
        return `<${node.tag} />`;
    }

    const props = (node?.props === undefined || node?.props === null) ? "" : " " + Object.entries(node.props).map(([k, v]) => `${k}="${v}"`).join(' ');
    return `<${node.tag}${props}>${node.children.map(render).join('')}</${node.tag}>`;
}
