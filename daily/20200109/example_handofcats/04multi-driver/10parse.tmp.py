file_input [9 children]
  simple_stmt [2 children]
    import_name [2 children]
      NAME('import') [lineno=1, column=0, prefix='']
      NAME('sys') [lineno=1, column=7, prefix=' ']
    NEWLINE('\n') [lineno=1, column=10, prefix='']
  simple_stmt [2 children]
    import_from [4 children]
      NAME('from') [lineno=2, column=0, prefix='']
      dotted_name [3 children]
        NAME('handofcats') [lineno=2, column=5, prefix=' ']
        DOT('.') [lineno=2, column=15, prefix='']
        NAME('driver') [lineno=2, column=16, prefix='']
      NAME('import') [lineno=2, column=23, prefix=' ']
      NAME('MultiDriver') [lineno=2, column=30, prefix=' ']
    NEWLINE('\n') [lineno=2, column=41, prefix='']
  simple_stmt [2 children]
    expr_stmt [3 children]
      NAME('md') [lineno=4, column=0, prefix='\n']
      EQUAL('=') [lineno=4, column=3, prefix=' ']
      power [2 children]
        NAME('MultiDriver') [lineno=4, column=5, prefix=' ']
        trailer [2 children]
          LPAR('(') [lineno=4, column=16, prefix='']
          RPAR(')') [lineno=4, column=17, prefix='']
    NEWLINE('\n') [lineno=4, column=18, prefix='']
  decorated [2 children]
    decorator [3 children]
      AT('@') [lineno=7, column=0, prefix='\n\n']
      dotted_name [3 children]
        NAME('md') [lineno=7, column=1, prefix='']
        DOT('.') [lineno=7, column=3, prefix='']
        NAME('register') [lineno=7, column=4, prefix='']
      NEWLINE('\n') [lineno=7, column=12, prefix='']
    funcdef[name='hello'] [5 children]
      NAME('def') [lineno=8, column=0, prefix='']
      NAME('hello') [lineno=8, column=4, prefix=' ']
      parameters [3 children]
        LPAR('(') [lineno=8, column=9, prefix='']
        typedargslist[args='*' ',' 'tname' '=' '"world"'] [5 children]
          STAR('*') [lineno=8, column=10, prefix='']
          COMMA(',') [lineno=8, column=11, prefix='']
          tname [3 children]
            NAME('name') [lineno=8, column=13, prefix=' ']
            COLON(':') [lineno=8, column=17, prefix='']
            NAME('str') [lineno=8, column=19, prefix=' ']
          EQUAL('=') [lineno=8, column=23, prefix=' ']
          STRING('"world"') [lineno=8, column=25, prefix=' ']
        RPAR(')') [lineno=8, column=32, prefix='']
      COLON(':') [lineno=8, column=33, prefix='']
      suite [4 children]
        NEWLINE('\n') [lineno=8, column=34, prefix='']
        INDENT('    ') [lineno=9, column=0, prefix='']
        simple_stmt [2 children]
          power [2 children]
            NAME('print') [lineno=9, column=4, prefix='']
            trailer [3 children]
              LPAR('(') [lineno=9, column=9, prefix='']
              STRING('f"hello {name}"') [lineno=9, column=10, prefix='']
              RPAR(')') [lineno=9, column=25, prefix='']
          NEWLINE('\n') [lineno=9, column=26, prefix='']
        DEDENT('') [lineno=12, column=0, prefix='\n\n']
  decorated [2 children]
    decorator [3 children]
      AT('@') [lineno=12, column=0, prefix='']
      dotted_name [3 children]
        NAME('md') [lineno=12, column=1, prefix='']
        DOT('.') [lineno=12, column=3, prefix='']
        NAME('register') [lineno=12, column=4, prefix='']
      NEWLINE('\n') [lineno=12, column=12, prefix='']
    funcdef[name='byebye'] [5 children]
      NAME('def') [lineno=13, column=0, prefix='']
      NAME('byebye') [lineno=13, column=4, prefix=' ']
      parameters [3 children]
        LPAR('(') [lineno=13, column=10, prefix='']
        NAME('name') [lineno=13, column=11, prefix='']
        RPAR(')') [lineno=13, column=15, prefix='']
      COLON(':') [lineno=13, column=16, prefix='']
      suite [4 children]
        NEWLINE('\n') [lineno=13, column=17, prefix='']
        INDENT('    ') [lineno=14, column=0, prefix='']
        simple_stmt [2 children]
          power [2 children]
            NAME('print') [lineno=14, column=4, prefix='']
            trailer [3 children]
              LPAR('(') [lineno=14, column=9, prefix='']
              STRING('f"byebye {name}"') [lineno=14, column=10, prefix='']
              RPAR(')') [lineno=14, column=26, prefix='']
          NEWLINE('\n') [lineno=14, column=27, prefix='']
        DEDENT('') [lineno=17, column=0, prefix='\n\n']
  simple_stmt [2 children]
    power [3 children]
      NAME('md') [lineno=17, column=0, prefix='']
      trailer [2 children]
        DOT('.') [lineno=17, column=2, prefix='']
        NAME('run') [lineno=17, column=3, prefix='']
      trailer [2 children]
        LPAR('(') [lineno=17, column=6, prefix='']
        RPAR(')') [lineno=17, column=7, prefix='']
    NEWLINE('\n') [lineno=17, column=8, prefix='']
  funcdef[name='main'] [5 children]
    NAME('def') [lineno=19, column=0, prefix='\n']
    NAME('main') [lineno=19, column=4, prefix=' ']
    parameters [3 children]
      LPAR('(') [lineno=19, column=8, prefix='']
      typedargslist[args='argv' '=' 'None'] [3 children]
        NAME('argv') [lineno=19, column=9, prefix='']
        EQUAL('=') [lineno=19, column=13, prefix='']
        NAME('None') [lineno=19, column=14, prefix='']
      RPAR(')') [lineno=19, column=18, prefix='']
    COLON(':') [lineno=19, column=19, prefix='']
    suite [19 children]
      NEWLINE('\n') [lineno=19, column=20, prefix='']
      INDENT('    ') [lineno=20, column=0, prefix='']
      simple_stmt [2 children]
        import_name [2 children]
          NAME('import') [lineno=20, column=4, prefix='']
          NAME('argparse') [lineno=20, column=11, prefix=' ']
        NEWLINE('\n') [lineno=20, column=19, prefix='']
      simple_stmt [2 children]
        expr_stmt [3 children]
          NAME('parser') [lineno=22, column=4, prefix='\n    ']
          EQUAL('=') [lineno=22, column=11, prefix=' ']
          power [3 children]
            NAME('argparse') [lineno=22, column=13, prefix=' ']
            trailer [2 children]
              DOT('.') [lineno=22, column=21, prefix='']
              NAME('ArgumentParser') [lineno=22, column=22, prefix='']
            trailer [2 children]
              LPAR('(') [lineno=22, column=36, prefix='']
              RPAR(')') [lineno=22, column=37, prefix='']
        NEWLINE('\n') [lineno=22, column=38, prefix='']
      simple_stmt [2 children]
        expr_stmt [3 children]
          NAME('subparsers') [lineno=23, column=4, prefix='    ']
          EQUAL('=') [lineno=23, column=15, prefix=' ']
          power [3 children]
            NAME('parser') [lineno=23, column=17, prefix=' ']
            trailer [2 children]
              DOT('.') [lineno=23, column=23, prefix='']
              NAME('add_subparsers') [lineno=23, column=24, prefix='']
            trailer [3 children]
              LPAR('(') [lineno=23, column=38, prefix='']
              arglist [3 children]
                argument [3 children]
                  NAME('title') [lineno=23, column=39, prefix='']
                  EQUAL('=') [lineno=23, column=44, prefix='']
                  STRING("'subcommands'") [lineno=23, column=45, prefix='']
                COMMA(',') [lineno=23, column=58, prefix='']
                argument [3 children]
                  NAME('dest') [lineno=23, column=60, prefix=' ']
                  EQUAL('=') [lineno=23, column=64, prefix='']
                  STRING("'subcommand'") [lineno=23, column=65, prefix='']
              RPAR(')') [lineno=23, column=77, prefix='']
        NEWLINE('\n') [lineno=23, column=78, prefix='']
      simple_stmt [2 children]
        expr_stmt [3 children]
          power [2 children]
            NAME('subparsers') [lineno=24, column=4, prefix='    ']
            trailer [2 children]
              DOT('.') [lineno=24, column=14, prefix='']
              NAME('required') [lineno=24, column=15, prefix='']
          EQUAL('=') [lineno=24, column=24, prefix=' ']
          NAME('True') [lineno=24, column=26, prefix=' ']
        NEWLINE('\n') [lineno=24, column=30, prefix='']
      simple_stmt [2 children]
        expr_stmt [3 children]
          NAME('fn') [lineno=26, column=4, prefix='\n    ']
          EQUAL('=') [lineno=26, column=7, prefix=' ']
          NAME('hello') [lineno=26, column=9, prefix=' ']
        NEWLINE('\n') [lineno=26, column=14, prefix='']
      simple_stmt [2 children]
        expr_stmt [3 children]
          NAME('sub_parser') [lineno=27, column=4, prefix='    ']
          EQUAL('=') [lineno=27, column=15, prefix=' ']
          power [3 children]
            NAME('subparsers') [lineno=27, column=17, prefix=' ']
            trailer [2 children]
              DOT('.') [lineno=27, column=27, prefix='']
              NAME('add_parser') [lineno=27, column=28, prefix='']
            trailer [3 children]
              LPAR('(') [lineno=27, column=38, prefix='']
              arglist [3 children]
                power [2 children]
                  NAME('fn') [lineno=27, column=39, prefix='']
                  trailer [2 children]
                    DOT('.') [lineno=27, column=41, prefix='']
                    NAME('__name__') [lineno=27, column=42, prefix='']
                COMMA(',') [lineno=27, column=50, prefix='']
                argument [3 children]
                  NAME('help') [lineno=27, column=52, prefix=' ']
                  EQUAL('=') [lineno=27, column=56, prefix='']
                  power [2 children]
                    NAME('fn') [lineno=27, column=57, prefix='']
                    trailer [2 children]
                      DOT('.') [lineno=27, column=59, prefix='']
                      NAME('__doc__') [lineno=27, column=60, prefix='']
              RPAR(')') [lineno=27, column=67, prefix='']
        NEWLINE('\n') [lineno=27, column=68, prefix='']
      simple_stmt [2 children]
        power [3 children]
          NAME('sub_parser') [lineno=28, column=4, prefix='    ']
          trailer [2 children]
            DOT('.') [lineno=28, column=14, prefix='']
            NAME('add_argument') [lineno=28, column=15, prefix='']
          trailer [3 children]
            LPAR('(') [lineno=28, column=27, prefix='']
            arglist [7 children]
              STRING("'--name'") [lineno=28, column=28, prefix='']
              COMMA(',') [lineno=28, column=36, prefix='']
              argument [3 children]
                NAME('required') [lineno=28, column=38, prefix=' ']
                EQUAL('=') [lineno=28, column=46, prefix='']
                NAME('False') [lineno=28, column=47, prefix='']
              COMMA(',') [lineno=28, column=52, prefix='']
              argument [3 children]
                NAME('default') [lineno=28, column=54, prefix=' ']
                EQUAL('=') [lineno=28, column=61, prefix='']
                STRING("'world'") [lineno=28, column=62, prefix='']
              COMMA(',') [lineno=28, column=69, prefix='']
              argument [3 children]
                NAME('help') [lineno=28, column=71, prefix=' ']
                EQUAL('=') [lineno=28, column=75, prefix='']
                STRING('"(default: \'world\')"') [lineno=28, column=76, prefix='']
            RPAR(')') [lineno=28, column=96, prefix='']
        NEWLINE('\n') [lineno=28, column=97, prefix='']
      simple_stmt [2 children]
        power [3 children]
          NAME('sub_parser') [lineno=29, column=4, prefix='    ']
          trailer [2 children]
            DOT('.') [lineno=29, column=14, prefix='']
            NAME('set_defaults') [lineno=29, column=15, prefix='']
          trailer [3 children]
            LPAR('(') [lineno=29, column=27, prefix='']
            argument [3 children]
              NAME('subcommand') [lineno=29, column=28, prefix='']
              EQUAL('=') [lineno=29, column=38, prefix='']
              NAME('fn') [lineno=29, column=39, prefix='']
            RPAR(')') [lineno=29, column=41, prefix='']
        NEWLINE('\n') [lineno=29, column=42, prefix='']
      simple_stmt [2 children]
        expr_stmt [3 children]
          NAME('fn') [lineno=31, column=4, prefix='\n    ']
          EQUAL('=') [lineno=31, column=7, prefix=' ']
          NAME('byebye') [lineno=31, column=9, prefix=' ']
        NEWLINE('\n') [lineno=31, column=15, prefix='']
      simple_stmt [2 children]
        expr_stmt [3 children]
          NAME('sub_parser') [lineno=32, column=4, prefix='    ']
          EQUAL('=') [lineno=32, column=15, prefix=' ']
          power [3 children]
            NAME('subparsers') [lineno=32, column=17, prefix=' ']
            trailer [2 children]
              DOT('.') [lineno=32, column=27, prefix='']
              NAME('add_parser') [lineno=32, column=28, prefix='']
            trailer [3 children]
              LPAR('(') [lineno=32, column=38, prefix='']
              arglist [3 children]
                power [2 children]
                  NAME('fn') [lineno=32, column=39, prefix='']
                  trailer [2 children]
                    DOT('.') [lineno=32, column=41, prefix='']
                    NAME('__name__') [lineno=32, column=42, prefix='']
                COMMA(',') [lineno=32, column=50, prefix='']
                argument [3 children]
                  NAME('help') [lineno=32, column=52, prefix=' ']
                  EQUAL('=') [lineno=32, column=56, prefix='']
                  power [2 children]
                    NAME('fn') [lineno=32, column=57, prefix='']
                    trailer [2 children]
                      DOT('.') [lineno=32, column=59, prefix='']
                      NAME('__doc__') [lineno=32, column=60, prefix='']
              RPAR(')') [lineno=32, column=67, prefix='']
        NEWLINE('\n') [lineno=32, column=68, prefix='']
      simple_stmt [2 children]
        power [3 children]
          NAME('sub_parser') [lineno=33, column=4, prefix='    ']
          trailer [2 children]
            DOT('.') [lineno=33, column=14, prefix='']
            NAME('add_argument') [lineno=33, column=15, prefix='']
          trailer [3 children]
            LPAR('(') [lineno=33, column=27, prefix='']
            STRING("'name'") [lineno=33, column=28, prefix='']
            RPAR(')') [lineno=33, column=34, prefix='']
        NEWLINE('\n') [lineno=33, column=35, prefix='']
      simple_stmt [2 children]
        power [3 children]
          NAME('sub_parser') [lineno=34, column=4, prefix='    ']
          trailer [2 children]
            DOT('.') [lineno=34, column=14, prefix='']
            NAME('set_defaults') [lineno=34, column=15, prefix='']
          trailer [3 children]
            LPAR('(') [lineno=34, column=27, prefix='']
            argument [3 children]
              NAME('subcommand') [lineno=34, column=28, prefix='']
              EQUAL('=') [lineno=34, column=38, prefix='']
              NAME('fn') [lineno=34, column=39, prefix='']
            RPAR(')') [lineno=34, column=41, prefix='']
        NEWLINE('\n') [lineno=34, column=42, prefix='']
      simple_stmt [2 children]
        expr_stmt [3 children]
          NAME('args') [lineno=36, column=4, prefix='\n    ']
          EQUAL('=') [lineno=36, column=9, prefix=' ']
          power [3 children]
            NAME('parser') [lineno=36, column=11, prefix=' ']
            trailer [2 children]
              DOT('.') [lineno=36, column=17, prefix='']
              NAME('parse_args') [lineno=36, column=18, prefix='']
            trailer [3 children]
              LPAR('(') [lineno=36, column=28, prefix='']
              NAME('argv') [lineno=36, column=29, prefix='']
              RPAR(')') [lineno=36, column=33, prefix='']
        NEWLINE('\n') [lineno=36, column=34, prefix='']
      simple_stmt [2 children]
        expr_stmt [3 children]
          NAME('params') [lineno=37, column=4, prefix='    ']
          EQUAL('=') [lineno=37, column=11, prefix=' ']
          power [4 children]
            NAME('vars') [lineno=37, column=13, prefix=' ']
            trailer [3 children]
              LPAR('(') [lineno=37, column=17, prefix='']
              NAME('args') [lineno=37, column=18, prefix='']
              RPAR(')') [lineno=37, column=22, prefix='']
            trailer [2 children]
              DOT('.') [lineno=37, column=23, prefix='']
              NAME('copy') [lineno=37, column=24, prefix='']
            trailer [2 children]
              LPAR('(') [lineno=37, column=28, prefix='']
              RPAR(')') [lineno=37, column=29, prefix='']
        NEWLINE('\n') [lineno=37, column=30, prefix='']
      simple_stmt [2 children]
        expr_stmt [3 children]
          NAME('subcommand') [lineno=38, column=4, prefix='    ']
          EQUAL('=') [lineno=38, column=15, prefix=' ']
          power [3 children]
            NAME('params') [lineno=38, column=17, prefix=' ']
            trailer [2 children]
              DOT('.') [lineno=38, column=23, prefix='']
              NAME('pop') [lineno=38, column=24, prefix='']
            trailer [3 children]
              LPAR('(') [lineno=38, column=27, prefix='']
              STRING("'subcommand'") [lineno=38, column=28, prefix='']
              RPAR(')') [lineno=38, column=40, prefix='']
        NEWLINE('\n') [lineno=38, column=41, prefix='']
      simple_stmt [2 children]
        return_stmt [2 children]
          NAME('return') [lineno=39, column=4, prefix='    ']
          power [2 children]
            NAME('subcommand') [lineno=39, column=11, prefix=' ']
            trailer [3 children]
              LPAR('(') [lineno=39, column=21, prefix='']
              argument [2 children]
                DOUBLESTAR('**') [lineno=39, column=22, prefix='']
                NAME('params') [lineno=39, column=24, prefix='']
              RPAR(')') [lineno=39, column=30, prefix='']
        NEWLINE('\n') [lineno=39, column=31, prefix='']
      DEDENT('') [lineno=42, column=0, prefix='\n\n']
  if_stmt [4 children]
    NAME('if') [lineno=42, column=0, prefix='']
    comparison [3 children]
      NAME('__name__') [lineno=42, column=3, prefix=' ']
      EQEQUAL('==') [lineno=42, column=12, prefix=' ']
      STRING("'__main__'") [lineno=42, column=15, prefix=' ']
    COLON(':') [lineno=42, column=25, prefix='']
    suite [4 children]
      NEWLINE('\n') [lineno=42, column=26, prefix='']
      INDENT('    ') [lineno=43, column=0, prefix='']
      simple_stmt [2 children]
        power [2 children]
          NAME('main') [lineno=43, column=4, prefix='']
          trailer [2 children]
            LPAR('(') [lineno=43, column=8, prefix='']
            RPAR(')') [lineno=43, column=9, prefix='']
        NEWLINE('\n') [lineno=43, column=10, prefix='']
      DEDENT('') [lineno=44, column=0, prefix='']
  ENDMARKER('') [lineno=44, column=0, prefix='']
