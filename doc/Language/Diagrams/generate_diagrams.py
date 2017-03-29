from railroad_diagrams import *

def expr():
    return NonTerminal('expr')

def select_stmt():
    return NonTerminal('select-stmt')

def name():
    return NonTerminal('name')

def bind_parameter():
    return NonTerminal('@bind-parameter')

def type_name():
    return NonTerminal('type-name')

def object_name():
    return NonTerminal('object-name')

def column_name():
    return NonTerminal('column-name')

def order_direction():
    return Optional(Choice(0, 'ASC', 'DESC'), 'skip')

def literal():
    return NonTerminal('literal')

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

def export(filename, diagram):
    with open(filename, 'w') as f:
        diagram.writeSvg(f.write)

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

export('CreateTable.svg', Diagram(Sequence(
    Stack(
        Sequence(
            'CREATE',
            Optional(Choice(0, 'TEMP', 'TEMPORARY'), 'skip'),
            'TABLE',
            object_name()),
        Choice(0,
            Sequence('AS', select_stmt()),
            Sequence(
                '(',
                ZeroOrMore(
                    Choice(0,
                        NonTerminal('column-def'),
                        NonTerminal('table-constraint')),
                    ','),
                ')'))))))

export('CreateView.svg', Diagram(
    Stack(
        Sequence(
            'CREATE',
            Optional(Choice(0, 'TEMP', 'TEMPORARY'), 'skip'),
            'VIEW',
            object_name()),
        Sequence(Optional(Sequence('(', ZeroOrMore(name(), ','), ')')), 'AS', select_stmt()))))

export('TypeName.svg', Diagram(
    Choice(0,
        Sequence(
            Choice(0, 'STRING', 'BINARY'),
            Optional(Sequence('(', NonTerminal('max-length'), ')'))),
        'INT8',
        'INT16',
        Choice(0, 'INT32', 'INT'),
        'INT64',
        'FLOAT32',
        Choice(0, 'FLOAT64', 'FLOAT'),
        'DECIMAL',
        'BOOL',
        'DATETIME',
        'DATETIMEOFFSET')))

export('Literal.svg', Diagram(
    Choice(0,
        'NULL',
        Choice(0, 'TRUE', 'FALSE'),
        NonTerminal('number'),
        Sequence("'", ZeroOrMore(Choice(0, NonTerminal('any-char-but-quote'), "''")), "'"),
        Sequence("x'", ZeroOrMore(NonTerminal('hex-digit-pair')), "'"),
        Sequence(
            NonTerminal('yyyy-MM-dd'),
            Optional(
                Sequence(
                    NonTerminal('THH:mm:ss'),
                    Optional(Sequence('.', NonTerminal('fff'))),
                    Optional(Sequence(Choice(0, '+', '-'), NonTerminal('HH:mm')))))))))

export('Expr.svg', Diagram(
    Choice(6,
        literal(),
        bind_parameter(),
        Sequence(
            '(',
            Choice(0, expr(), select_stmt()),
            ')'),
        Sequence(
            'CAST',
            '(',
            expr(),
            'AS',
            type_name(),
            ')'),
        Sequence(
            Choice(0, 'NOT', '~', '-', '+'),
            expr()),
        Sequence(expr(),
            Choice(1,
                Sequence('COLLATE', name()),
                Sequence(NonTerminal('binary-operator'), expr()),
                Sequence(
                    Optional('NOT', 'skip'),
                    'LIKE',
                    expr(),
                    Optional(Sequence('ESCAPE', expr()), 'skip')),
                Sequence(
                    Optional('NOT', 'skip'),
                    'BETWEEN',
                    expr(),
                    'AND',
                    expr()),
                Sequence(
                    Optional('NOT', 'skip'),
                    'IN',
                    Choice(1,
                        object_name(),
                        Sequence(
                            '(',
                            Choice(0,
                                ZeroOrMore(expr(), ','),
                                select_stmt()),
                            ')'),
                        bind_parameter())))),
        Sequence(
            NonTerminal('function-name'),
            '(',
            Choice(0,
                Sequence(Optional('DISTINCT', 'skip'), ZeroOrMore(expr(), ',')),
                '*'),
            ')'),
        column_name(),
        NonTerminal('case-expr'),
        Sequence(
            'EXISTS',
            '(',
            select_stmt(),
            ')'))))


