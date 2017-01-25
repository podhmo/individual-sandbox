# -*- coding:utf-8 -*-
from prestring import go
from collections import namedtuple
"""
input

  workbook: [sheet0, sheet1, sheet2]
  attributes: ["file", "item"]

syntax

 <sheet> :: <row> <row>*
 <row> :: <cell> <cell>*
 <cell> :: <const> | <access>
 <const> :: "const" <word> <theme>?
 <access> :: "access" <raw> <theme>?

# todo: theme
"""

Cell = namedtuple("Cell", "type, code, theme")  # TODO: use
C = "const"
A = "access"


def split_action(action):
    if action == A or action == C:
        return action, str
    elif action.startswith(A):
        return A, detect_type(action[len(A):])
    elif action.startswith(C):
        return C, detect_type(action[len(C):])
    else:
        raise ValueError("invalid action: {}".format(action))


def detect_type(suffix):
    if suffix.startswith("i"):
        return int
    elif suffix.startswith("f"):
        return float
    else:
        return str


def generate_all(filename, workbook, m=None):
    m = m or go.Module()
    m.package("main")
    with m.import_group() as im:
        im("fmt")
        im("github.com/tealeg/xlsx")
    m.sep()

    with m.func("main"):
        m.stmt('err := WriteFile("{filename}")'.format(filename=filename))
        with m.if_('err != nil'):
            m.stmt('panic(err)')

    generate_write_workbook('WriteFile', workbook, m=m)
    for i, sheet in enumerate(workbook):
        sheetname = sheet.get('name') or 'sheet{i}'.format(i=i)
        generate_write_sheet(sheet, sheetname=sheetname, m=m)
    return m


def generate_write_workbook(funcname, workbook, m=None):
    m = m or go.Module()
    m.stmt("// {funcname} :".format(funcname=funcname))
    with m.func(funcname, 'name string', return_='error'):
        m.stmt("// todo: set font")
        m.stmt("file := xlsx.NewFile()")
        for i, sheet in enumerate(workbook):
            sheetname = sheet.get('name') or 'sheet{i}'.format(i=i)
            m.stmt('sheet{i}, err := file.AddSheet("{sheetname}")'.format(i=i, sheetname=sheetname))
            with m.if_('err != nil'):
                m.return_('err')

            writefunc = sheet.get('func') or 'Write{sheetname}'.format(sheetname=go.titlize(sheetname))
            args = [sheetname]
            m.stmt('// todo: arguments')
            with m.if_('err := {writefunc}({args}); err != nil'.format(writefunc=writefunc, args=', '.join(args))):
                m.return_('err')
            m.sep()
        m.return_('file.Save(name)')
    return m


def generate_write_sheet(sheet, sheetname=None, m=None):
    m = m or go.Module()
    funcname = sheet.get('func') or 'Write{sheetname}'.format(sheetname=go.titlize(sheetname))

    f = sheet['attributes'][0][0]
    m.stmt("// {funcname} :".format(funcname=funcname))
    with m.func(funcname, *[' '.join(pair) for pair in sheet['attributes']], return_=sheet.get('returns') or 'error'):
        for i, row in enumerate(sheet['sheet']):
            m.stmt('row{i} := {sheet}.AddRow()'.format(i=i, sheet=f))
            for j, cell in enumerate(row):
                m.stmt('cell{i}{j} := row{i}.AddCell()'.format(i=i, j=j))

                action, value, theme = None, None, None
                if len(cell) == 1:
                    value, = cell
                elif len(cell) == 2:
                    action, value = cell
                else:
                    action, value, theme, *_ = cell

                raw_action, typ = split_action(action)
                if raw_action == C:
                    if typ == str:
                        m.stmt('cell{i}{j}.SetString("{value}")'.format(i=i, j=j, value=value))
                    elif typ == float:
                        m.stmt('cell{i}{j}.SetFloat({value})'.format(i=i, j=j, value=value))
                    else:
                        raise ValueError(raw_action, typ)
                elif raw_action == A:
                    if typ == str:
                        m.stmt('cell{i}{j}.SetString({value})'.format(i=i, j=j, value=value))
                    elif typ == float:
                        m.stmt('cell{i}{j}.SetFloat({value})'.format(i=i, j=j, value=value))
                    else:
                        raise ValueError(raw_action, typ)
                else:
                    raise ValueError('invalid action: {} in {}'.format(action, cell))
            m.sep()
        # todo: return value
        m.return_('nil')
    return m


def main_default():
    workbook = [{
        "attributes": [("sheet", "*xlsx.Sheet")],
        "sheet": [
            [[C, 1], [C, 2], [C, 3], [C, 4], [C, 5]],
            [[C, 1], [C, 2 * 2], [C, 3 * 3], [C, 4 * 4], [C, 5 * 5]],
        ],
        "func": "WriteXS",
    }]
    print(generate_write_sheet(workbook[0]))


if __name__ == '__main__':
    import argparse
    import os.path
    from dictknife import loading

    parser = argparse.ArgumentParser()
    parser.add_argument('file')
    try:
        args = parser.parse_args()
    except:
        main_default()
        import sys
        sys.exit(0)

    with open(args.file, 'r') as rf:
        workbook = loading.load(rf)
        outname = os.path.basename(os.path.splitext(args.file)[0] + '.xlsx')
        print(generate_all(outname, workbook))

