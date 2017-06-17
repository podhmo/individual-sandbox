# https://github.com/cgwrench/rst2md/blob/master/markdown.py
from docutils import frontend, nodes, writers, languages


class Writer(writers.Writer):

    supported = ('markdown', )
    output = None
    settings_spec = (
        'Markdown-Specific Options', None, (
            (
                'Extended Markdown syntax.', ['--extended-markdown'],
                {
                    'default': 0,
                    'action': 'store_true',
                    'validator': frontend.validate_boolean
                }
            ), (
                'Strict Markdown syntax. Default: true', ['--strict-markdown'],
                {
                    'default': 1,
                    'action': 'store_true',
                    'validator': frontend.validate_boolean
                }
            ),
        )
    )

    def __init__(self):
        writers.Writer.__init__(self)
        self.translator_class = Translator

    def translate(self):
        visitor = self.translator_class(self.document)
        self.document.walkabout(visitor)
        self.output = visitor.astext()


class Translator(nodes.NodeVisitor):
    def __init__(self, document):
        nodes.NodeVisitor.__init__(self, document)
        self.settings = settings = document.settings
        lcode = settings.language_code
        self.language = languages.get_language(lcode, document.reporter)

        self.head = []
        self.body = []
        self.foot = []

        self.section_level = 0

        ##TODO docinfo items can go in a footer HTML element (store in self.foot).
        self._docinfo = {
            'title': '',
            'subtitle': '',
            'author': [],
            'date': '',
            'copyright': '',
            'version': '',
        }

        # Customise Markdown syntax here. Still need to add literal, term,
        # indent, problematic etc...
        self.defs = {
            'emphasis': ('*', '*'),  # Could also use ('_', '_')
            'problematic': ('\n\n', '\n\n'),
            'strong': ('**', '**'),  # Could also use ('__', '__')
            'subscript': ('<sub>', '</sub>'),
            'superscript': ('<sup>', '</sup>'),
        }

    # Utility methods

    def astext(self):
        """Return the final formatted document as a string."""
        return ''.join(self.head + self.body + self.foot)

    def deunicode(self, text):
        text = text.replace(u'\xa0', '\\ ')
        text = text.replace(u'\u2020', '\\(dg')
        return text

    def ensure_eol(self):
        """Ensure the last line in body is terminated by new line."""
        if self.body and self.body[-1][-1] != '\n':
            self.body.append('\n')

    # Node visitor methods

    def visit_Text(self, node):
        text = node.astext()
        self.body.append(text)

    def depart_Text(self, node):
        pass

    def visit_comment(self, node):
        self.body.append('<!-- ' + node.astext() + ' -->\n')
        raise nodes.SkipNode

    def visit_docinfo_item(self, node, name):
        if name == 'author':
            self._docinfo[name].append(node.astext())
        else:
            self._docinfo[name] = node.astext()
        raise nodes.SkipNode

    def visit_document(self, node):
        pass

    def depart_document(self, node):
        pass

    def visit_emphasis(self, node):
        self.body.append(self.defs['emphasis'][0])

    def depart_emphasis(self, node):
        self.body.append(self.defs['emphasis'][1])

    def visit_paragraph(self, node):
        self.ensure_eol()
        self.body.append('\n')

    def depart_paragraph(self, node):
        self.body.append('\n')

    def visit_problematic(self, node):
        self.body.append(self.defs['problematic'][0])

    def depart_problematic(self, node):
        self.body.append(self.defs['problematic'][1])

    def visit_section(self, node):
        self.section_level += 1

    def depart_section(self, node):
        self.section_level -= 1

    def visit_strong(self, node):
        self.body.append(self.defs['strong'][0])

    def depart_strong(self, node):
        self.body.append(self.defs['strong'][1])

    def visit_subscript(self, node):
        self.body.append(self.defs['subscript'][0])

    def depart_subscript(self, node):
        self.body.append(self.defs['subscript'][1])

    def visit_subtitle(self, node):
        if isinstance(node.parent, nodes.document):
            self.visit_docinfo_item(node, 'subtitle')
            raise SkipNode

    def visit_superscript(self, node):
        self.body.append(self.defs['superscript'][0])

    def depart_superscript(self, node):
        self.body.append(self.defs['superscript'][1])

    def visit_system_message(self, node):
        # TODO add report_level
        #if node['level'] < self.document.reporter['writer'].report_level:
        #    Level is too low to display:
        #    raise nodes.SkipNode
        attr = {}
        backref_text = ''
        if node.hasattr('id'):
            attr['name'] = node['id']
        if node.hasattr('line'):
            line = ', line %s' % node['line']
        else:
            line = ''
        self.body.append(
            '"System Message: %s/%s (%s:%s)"\n' %
            (node['type'], node['level'], node['source'], line)
        )

    def depart_system_message(self, node):
        pass

    def visit_title(self, node):
        if self.section_level == 0:
            self.head.append('# {0}\n'.format(node.astext()))
            self._docinfo['title'] = node.astext()
            raise nodes.SkipNode
        else:
            self.body.append(
                '{0} {1}\n'.format((self.section_level + 1) * '#', self.deunicode(node.astext()))
            )
            raise nodes.SkipNode

    def depart_title(self, node):
        self.body.append('\n')

    def visit_transition(self, node):
        # Simply replace a transition by a horizontal rule.
        # Could use three or more '*', '_' or '-'.
        self.body.append('\n---\n\n')
        raise nodes.SkipNode


# The following code adds visit/depart methods for any reSturcturedText element
# which we have not explicitly implemented above.

# All reStructuredText elements:
rst_elements = (
    'abbreviation', 'acronym', 'address', 'admonition', 'attention', 'attribution', 'author',
    'authors', 'block_quote', 'bullet_list', 'caption', 'caution', 'citation', 'citation_reference',
    'classifier', 'colspec', 'comment', 'compound', 'contact', 'container', 'copyright', 'danger',
    'date', 'decoration', 'definition', 'definition_list', 'definition_list_item', 'description',
    'docinfo', 'doctest_block', 'document', 'emphasis', 'entry', 'enumerated_list', 'error',
    'field', 'field_body', 'field_list', 'field_name', 'figure', 'footer', 'footnote',
    'footnote_reference', 'generated', 'header', 'hint', 'image', 'important', 'inline', 'label',
    'legend', 'line', 'line_block', 'list_item', 'literal', 'literal_block', 'math', 'math_block',
    'note', 'option', 'option_argument', 'option_group', 'option_list', 'option_list_item',
    'option_string', 'organization', 'paragraph', 'pending', 'problematic', 'raw', 'reference',
    'revision', 'row', 'rubric', 'section', 'sidebar', 'status', 'strong', 'subscript',
    'substitution_definition', 'substitution_reference', 'subtitle', 'superscript',
    'system_message', 'table', 'target', 'tbody,'
    'term', 'tgroup', 'thead', 'tip', 'title', 'title_reference', 'topic', 'transition', 'version',
    'warning',
)

##TODO Eventually we should silently ignore unsupported reStructuredText 
##     constructs and document somewhere that they are not supported.
##     In the meantime raise a warning *once* for each unsupported element.
_warned = set()


def visit_unsupported(self, node):
    node_type = node.__class__.__name__
    if node_type not in _warned:
        self.document.reporter.warning('The ' + node_type + \
            ' element is not supported.')
        _warned.add(node_type)
    raise nodes.SkipNode


for element in rst_elements:
    if not hasattr(Translator, 'visit_' + element):
        setattr(Translator, 'visit_' + element, visit_unsupported)
