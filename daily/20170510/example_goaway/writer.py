from goaway.writer import Writer, EnumWriter


class TinyEnumWriter(EnumWriter):
    def write(self, enum, file, m):
        self.write_definition(enum, file, m)
        # self.write_string_method(enum, file, m)
        # self.write_parse_method(enum, file, m)
        return m


class MyWriter(Writer):
    enum_writer_factory = TinyEnumWriter
