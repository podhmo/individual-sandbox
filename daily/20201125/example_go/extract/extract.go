package extract

import (
	"fmt"
	"strings"
	"text/template"
	"text/template/parse"
	_ "unsafe"
)

//go:linkname template_builtins text/template.builtins
func template_builtins() template.FuncMap

func Extract(source string, fMap template.FuncMap) error {
	if fMap == nil {
		fMap = template_builtins()
	}
	t, err := parse.Parse("X", source, "", "", fMap)
	if err != nil {
		return err
	}
	for _, tree := range t {
		walkTree(tree)
	}
	return nil
}

func walkTree(t *parse.Tree) {
	// TODO: with walking history (for debug)
	var walkNode func(node parse.Node, depth int)
	walkNode = func(node parse.Node, depth int) {
		switch node := node.(type) {
		case *parse.ListNode:
			for _, subnode := range node.Nodes {
				walkNode(subnode, depth+1)
			}
		case *parse.TextNode:
		case *parse.PipeNode:
			for _, x := range node.Decl {
				walkNode(x, depth+1)
			}
			for _, x := range node.Cmds {
				walkNode(x, depth+1)
			}
		case *parse.ActionNode:
			walkNode(node.Pipe, depth+1)
		case *parse.CommandNode:
			for _, x := range node.Args {
				walkNode(x, depth+1)
			}
		case *parse.IdentifierNode:
			fmt.Println(strings.Repeat("  ", depth), "identifier Ident", node.Ident)
		case *parse.VariableNode:
			fmt.Println(strings.Repeat("  ", depth), "variable Ident", node.Ident)
		case *parse.DotNode:
		case *parse.NilNode:
		case *parse.FieldNode:
			fmt.Println(strings.Repeat("  ", depth), "field Ident", node.Ident)
		case *parse.ChainNode:
		case *parse.BoolNode:
		case *parse.NumberNode:
		case *parse.StringNode:
		case *parse.BranchNode:
			walkNode(node.Pipe, depth+1)
			if node.List != nil {
				walkNode(node.List, depth+1)
			}
			if node.ElseList != nil {
				walkNode(node.ElseList, depth+1)
			}
		case *parse.IfNode:
			walkNode(&node.BranchNode, depth+1)
		case *parse.RangeNode:
			walkNode(&node.BranchNode, depth+1)
		case *parse.WithNode:
			walkNode(&node.BranchNode, depth+1)
		case *parse.TemplateNode:
			walkNode(node.Pipe, depth+1) // ??
		default:
			// skip endNode, elseNode
		}
	}
	for _, node := range t.Root.Nodes {
		walkNode(node, 0)
	}
}
