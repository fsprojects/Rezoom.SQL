from railroad_diagrams import *

def expr():
    return NonTerminal('expr')

def name():
    return NonTerminal('name')

def type_name():
    return NonTerminal('type-name')

def object_name():
    return NonTerminal('object-name')

def order_direction():
    return Optional(Choice(0, 'ASC', 'DESC'), 'skip')

def export(filename, diagram):
    with open(filename, 'w') as f:
        diagram.writeSvg(f.write)

def primary_key_clause():
    return Sequence(
        'PRIMARY',
        'KEY',
        order_direction(),
        Optional('AUTOINCREMENT', 'skip'))

def foreign_key_clause():
    return Sequence(
        'REFERENCES',
        object_name(),
        '(',
        ZeroOrMore(name(), ','),
        ')')

export('ColumnDef.svg', Diagram(
    name(),
    type_name(),
    ZeroOrMore(
        Sequence(
            Optional(Sequence('CONSTRAINT', name()), 'skip'),
            Choice(0,
                'NULL',
                'UNIQUE',
                Sequence('DEFAULT', expr()),
                Sequence('COLLATE', name()),
                primary_key_clause(),
                foreign_key_clause())))))

export('TableConstraint.svg', Diagram(
    Stack(
        Optional(Sequence('CONSTRAINT', name()), 'skip'),
        Choice(0,
            Sequence('FOREIGN', 'KEY', '(', ZeroOrMore(name(), ','), ')', foreign_key_clause()),
            Sequence('CHECK', '(', expr(), ')'),
            Sequence(
                Choice(0, 'UNIQUE', Sequence('PRIMARY', 'KEY')),
                '(',
                OneOrMore(Sequence(name(), order_direction()), ','),
                ')')))))

export('CreateTable.svg', Diagram(
    Stack(
        Sequence(
            'CREATE',
            Optional(Choice(0, 'TEMP', 'TEMPORARY'), 'skip'),
            'TABLE',
            object_name()),
        Choice(0,
            Sequence('AS', NonTerminal('select-statement')),
            Sequence(
                '(',
                ZeroOrMore(
                    Choice(0,
                        NonTerminal('column-def'),
                        NonTerminal('table-constraint')),
                    ','),
                ')')))))

