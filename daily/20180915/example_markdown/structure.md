## docutils?

```console
$ pyinspect inspect recommonmark.parser:CommonMarkParser
recommonmark.parser.CommonMarkParser <- docutils.parsers.Parser <- docutils.Component <- docutils.SettingsSpec <- docutils.TransformSpec <- builtins.object
    [method, OVERRIDE] parse(self, inputstring, document)
        [method] convert_block(self, block)
            [method] convert_blocks(self, blocks)
            [method] section(self, block)
            [method] paragraph(self, block)
            [method] blockquote(self, block)
                [method] _temp_current_node(self, current_node)
                [method] convert_blocks(self, blocks)
            [method] list_item(self, block)
                [method] _temp_current_node(self, current_node)
                [method] convert_blocks(self, blocks)
            [method] list_block(self, block)
                [method] _temp_current_node(self, current_node)
                [method] convert_blocks(self, blocks)
            [method] verbatim(self, text)
            [method] code(self, language, text)
            [method] reference(self, block)
            [method] horizontal_rule(self)
            [method] html_block(self, block)

docutils.parsers.Parser <- docutils.Component <- docutils.SettingsSpec <- docutils.TransformSpec <- builtins.object
    [method] finish_parse(self)
    [method] parse(self, inputstring, document)
    [method] setup_parse(self, inputstring, document)

docutils.Component <- docutils.SettingsSpec <- docutils.TransformSpec <- builtins.object
    [method] supports(self, format)

docutils.SettingsSpec <- builtins.object

docutils.TransformSpec <- builtins.object
    [method] get_transforms(self)
```
