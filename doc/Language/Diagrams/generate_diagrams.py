from railroad_diagrams import *

def export(filename, diagram):
    with open(filename, 'w') as f:
        diagram.writeSvg(f.write)

export('CreateTable.svg', Diagram(
    Stack(
        Sequence(
            'CREATE',
            Optional(Choice(0, 'TEMP', 'TEMPORARY')),
            'TABLE',
            NonTerminal('object-name')),
        Choice(0,
            Sequence('AS', NonTerminal('select-statement')),
            Sequence(
                '(',
                ZeroOrMore(
                    Choice(0,
                        NonTerminal('column-def'),
                        NonTerminal('table-constraint')),
                    ','),
                ')')))));

