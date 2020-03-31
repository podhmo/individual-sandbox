import graphql
from graphql.language import print_ast


document_node = graphql.parse(
    """
{
  root { name }
}
"""
)
print(f"{document_node.__class__.__module__}.{document_node.__class__.__name__}")
print(print_ast(document_node))
