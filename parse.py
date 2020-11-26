from lark import Lark, Token, Transformer, v_args
import re
import sys

class Location(object):
    def __init__(self, line, col):
        self.line = line
        self.col = col

    def __str__(self):
        return '{}:{}'.format(self.line, self.col)

    def __repr__(self):
        return 'Location({}, {})'.format(self.line, self.col)

    @staticmethod
    def from_meta(meta):
        return Location(meta.line, meta.column), Location(meta.end_line, meta.end_column)

class Chunk(object):
    def __init__(self, value, start=None, end=None):
        if isinstance(value, Token):
            if start is None: start = Location(value.line, value.column)
            if end is None: end = Location(value.end_line, value.end_column)
        elif not isinstance(value, str): assert False

        self.value = value
        self.start = start
        self.end = end

    def with_meta(self, meta):
        start, end = Location.from_meta(meta)
        return Chunk(self.value, start, end)

    @staticmethod
    def start(value):
        if isinstance(value, Chunk):
            return value.start
        elif isinstance(value, Token):
            return Location(value.line, value.column)
        else: assert False

    @staticmethod
    def end(value):
        if isinstance(value, Chunk):
            return value.end
        elif isinstance(value, Token):
            return Location(value.end_line, value.end_column)
        else: assert False

    def __add__(self, other):
        if isinstance(other, str):
            return Chunk(self.value + other, self.start, self.end)
        elif isinstance(other, Chunk):
            return Chunk(self.value + other.value, self.start, other.end)
        else: assert False

    def __getitem__(self, index):
        return Chunk(self.value.__getitem__(index), self.start, self.end)

    def __len__(self):
        return len(self.value)

    def join(self, args):
        if len(args) == 0: return Chunk('')
        return Chunk(self.value.join([arg.value for arg in args]), Chunk.start(args[0]), Chunk.end(args[-1]))

    def format(self, *args):
        return Chunk(self.value.format(*[arg.value for arg in args]), Chunk.start(args[0]), Chunk.end(args[-1]))

    def replace(self, x, y):
        return Chunk(self.value.replace(x, y), self.start, self.end)

    def indent(self):
        return Chunk(indent(self.value), self.start, self.end)

    def __repr__(self):
        return 'Chunk({!r}, {!r}, {!r})'.format(self.value, self.start, self.end)

    def __str__(self):
        return self.value

    @staticmethod
    def join_lines(lines, indent=True):
        if not isinstance(lines, list):
            lines = list(lines)

        if len(lines) == 0: return Chunk('')

        pieces = []
        for line, next_line in zip(lines, lines[1:]):
            distance = 1
            if line.end is not None and next_line.start is not None:
                distance = max(next_line.start.line - line.end.line, 1)
            pieces.append(line.value)
            pieces.append('\n' * distance)

        pieces.append(lines[-1].value)

        result = Chunk(''.join(pieces), lines[0].start, lines[-1].end)

        if indent: return result.indent()
        else: return result

INDENT = ' ' * 4
def indent(s):
    return re.sub(r'^(?!$)', INDENT, s, flags=re.MULTILINE)

def literal(self, value):
    return value if isinstance(value, Chunk) else Chunk(value)

def passthru(self, children):
    return children[0]

def smoosh(self, children):
    return Chunk('').join(children)

def spaces(self, children):
    return Chunk(' ').join(children)

def camelcase(s):
    return re.sub(r'(?!^)_([a-zA-Z])', lambda m: m.group(1).upper(), s)

def formatted(format_string):
    def result(self, children):
        return Chunk(format_string).format(*children)
    return result

class Rewriter(Transformer):
    ENUMERATED_TYPE = literal
    ENUMERATED_TYPES = literal
    PRIMITIVE_TYPE = literal
    BOOLEAN = literal
    NUMBER = literal
    STRING_LITERAL = literal
    UNARY_OPERATOR = literal
    PREFIX_UNARY_OPERATOR = literal

    # TEMPLATE_CHARACTERS: /(\\{|[^{])+/
    TEMPLATE_CHARACTERS = literal
    # template_head: "`" TEMPLATE_CHARACTERS? "{"
    def template_head(self, children):
        return Chunk('`{}${{').format(children[0] if children else Chunk(''))
    # template_middle: "}" TEMPLATE_CHARACTERS? "{"
    def template_middle(self, children):
        return Chunk('}}{}${{').format(children[0] if children else Chunk(''))
    # template_tail: "}" TEMPLATE_CHARACTERS? "`"
    def template_tail(self, children):
        return Chunk('}}{}`').format(children[0] if children else Chunk(''))

    # template_substituted: template_head expression ( template_middle expression )* template_tail
    def template_substituted(self, children):
        return Chunk('').join(children)

    # template_pure: "`" TEMPLATE_CHARACTERS "`"
    def template_pure(self, children):
        return Chunk('`{}`').format(children[0] if children else Chunk(''))

    # template_literal: template_pure | template_substituted
    template_literal = passthru

    def IDENTIFIER(self, id):
        prefix = "Lib." if literal(self, id).value in ashref else ""
        return Chunk(prefix + camelcase(literal(self, id).value))

    def BINARY_OPERATOR(self, op):
        map = { '==': '===', '!=': '!==' }
        return Chunk('{}').format(Chunk(op) + Chunk('=') if op in map else op)

    def COMMENT(self, *args): assert False

    def STRING_LITERAL(self, value):
        return Chunk('"{}"').format(Chunk(value)[1:-1])

    # function_call: IDENTIFIER "(" ( expression ( "," ( expression? ) )* )? ")"
    def function_call(self, children):
        name, *rest = children
        return Chunk('{}({})').format(name, Chunk(', ').join(rest))

    # index_expression: expression "[" expression "]"
    def index_expression(self, children):
        aggregate, index = children
        return Chunk('{}[{}]').format(aggregate, index)

    # unary_expression: ( PREFIX_UNARY_OPERATOR | UNARY_OPERATOR ) expression
    # unary_postfix_expression: expression UNARY_OPERATOR
    unary_expression = smoosh
    unary_postfix_expression = smoosh

    # binary_expression: expression BINARY_OPERATOR expression
    binary_expression = spaces

    # ternary_expression: expression "?" expression ":" expression
    ternary_expression = formatted('{} ? {} : {}')

    # paren_expression: "(" expression ")"
    paren_expression = formatted('({})')

    # field_expression: expression "." IDENTIFIER
    field_expression = formatted('{}.{}')

    # method_expression: expression "." function_call
    def method_expression(self, children):
        expression, func = children
        func.value = func.value.replace('(', '(' + expression.value + ', ', 1)
        func.value = func.value.replace(', )', ')')
        return func

    # call_expression: "call" function_call
    call_expression = formatted('call {}')

    # new_expression: "new" function_call
    new_expression = formatted('new {}')

    # enum_name: /(\\[[0-9]+\\])?[a-zA-Z0-9-&; '"?,\\\\()\\.*:!\\/í_]+/
    enum_name = passthru

    # singular_enum_expression: "$" ENUMERATED_TYPE "[" enum_name "]"
    singular_enum_expression = formatted('${}`{}`')

    # plural_enum_expression: "$" ENUMERATED_TYPES "[" ( enum_name ( "," enum_name )* )? "]"
    def plural_enum_expression(self, children):
        print(children)
        typ, *rest = children
        return Chunk('${}`{}`').format(typ, Chunk(', ').join(rest))

    # enum_expression: singular_enum_expression | plural_enum_expression
    enum_expression = passthru

    # expression: binary_expression | unary_expression | unary_postfix_expression | ternary_expression | enum_expression | method_expression | field_expression | index_expression | paren_expression | function_call | call_expression | new_expression | NUMBER | STRING_LITERAL | template_literal | BOOLEAN | IDENTIFIER
    expression = passthru

    # index_type: PRIMITIVE_TYPE | ENUMERATED_TYPE | /[0-9]+/
    index_type = passthru

    # aggregate_type: type "[" index_type ( "," index_type )* "]"
    def aggregate_type(self, children):
        typ, *indices = children
        return Chunk('{}[{}]').format(typ, Chunk(', ').join([index for index in indices]))

    # type: PRIMITIVE_TYPE | ENUMERATED_TYPE | aggregate_type | IDENTIFIER
    type = passthru

    # return_type: "void" | type
    def return_type(self, children):
        return children[0] if len(children) > 0 else Chunk('void')

    # dict_literal_entry: ( NUMBER | STRING_LITERAL | BOOLEAN | enum_expression ) ":" expression
    dict_literal_entry = formatted('{}: {}')

    # dict_literal: "{" dict_literal_entry ( "," dict_literal_entry )* ","? "}"
    def dict_literal(self, children):
        return Chunk('{{{}}}').format(Chunk(', ').join(children))

    # array_literal: "{" ( expression ( "," expression )* )? ","? "}"
    array_literal = dict_literal

    # remove_statement: "remove" index_expression ";"
    remove_statement = formatted('remove {};')

    # sort_statement: "sort" expression "by" expression ";"
    sort_statement = formatted('sort {} by {};')

    # since_statement: "since" NUMBER ";"
    since_statement = formatted('since {};')

    # expression_statement: expression ";"
    expression_statement = formatted('{};')

    # if_statement: "if" "(" expression ")" block_or_statement ( "else" block_or_statement )?
    def if_statement(self, children):
        condition, body, *rest = children
        return Chunk('if ({}) {}').format(condition, body) + (Chunk(' ') if body.value.endswith('}') else Chunk('\n')) + (Chunk('else {}').format(rest[0]) if len(rest) > 0 else Chunk(''))

    # while_statement: "while" "(" expression ")" block_or_statement
    while_statement = formatted('while ({}) {}')

    # for_statement: "for" IDENTIFIER "from" expression "to" expression ( "by" expression )? block_or_statement
    def for_statement(self, children):
        if len(children) == 4:
            bind, start, end, body = children
            return Chunk('for {} from {} to {} {}').format(bind, start, end, body)
        else:
            bind, start, end, by, body = children
            return Chunk('for {} from {} to {} by {} {}').format(bind, start, end, by, body)

    # foreach_statement: "foreach" IDENTIFIER ( "," IDENTIFIER )* "in" expression block_or_statement
    def foreach_statement(self, children):
        names = children[:-2]
        aggregate, body = children[-2:]
        return Chunk('for (const {} of {}) {}').format(literal(self, names[0]), aggregate, body)

    # case: ( "case" expression | "default" ) ":" ( block_or_statement )*
    def case(self, children):
        matcher, *rest = children
        return Chunk('case {}:\n{}').format(matcher, Chunk.join_lines(rest))

    # switch_statement: "switch" "(" expression ")" "{" case* "}"
    @v_args(meta=True)
    def switch_statement(self, children, meta):
        switcher, *rest = children
        return Chunk('switch ({}) {{\n{}\n}}').format(switcher, Chunk.join_lines(rest)).with_meta(meta)

    # static_statement: "static" block
    static_statement = formatted('static {}')

    # return_statement: "return" expression? ";"
    def return_statement(self, children):
        return Chunk('return {};').format(children[0]) if len(children) > 0 else Chunk('return;')

    # typedef_statement: "typedef" type IDENTIFIER ";"
    typedef_statement = formatted('typedef {} {};')

    # variable_declaration_statement: type IDENTIFIER ( "=" expression | "="? ( dict_literal | array_literal ) )? ";"
    def variable_declaration_statement(self, children):
        declaration, *rest = children
        return declaration + (Chunk(' = {};').format(rest[0]) if len(rest) > 0 else Chunk(';'))

    # variable_declaration: type IDENTIFIER
    def variable_declaration(self, children):
        typ, identifier = children
        return Chunk('const {}').format(identifier)

    def argument_declaration(self, children):
        typ, identifier = children
        if typ.value in ['int', 'float']: result = 'number'
        elif typ.value in ['boolean', 'string']: result = typ.value
        else: result = typ.value.capitalize()
        return Chunk('{}: {}').format(identifier, Chunk(result))

    # function_declaration: return_type IDENTIFIER "(" ( variable_declaration ( "," variable_declaration )* )? ")" block
    def function_declaration(self, children):
        ret, name, *arguments = children[:-1]
        body = children[-1]
        return Chunk('function {}({}) {}').format(name, Chunk(', ').join(arguments), body)

    # record_declaration: "record"i IDENTIFIER "{" ( variable_declaration ";" )+ "}" ";"
    @v_args(meta=True)
    def record_declaration(self, children, meta):
        name, *rest = children
        return Chunk('') # Chunk('record {} {{\n{}\n}}').format(name, Chunk.join_lines(rest)).with_meta(meta)

    # import_statement: "import" ( STRING_LITERAL | /<.*?(?<!\\\\)>/ )
    def import_statement(self, children):
        text, = children
        return Chunk('import "{}";').format(literal(self, text)[1:-1].replace(".ash", ".ts"))

    # statement: if_statement | while_statement | for_statement | foreach_statement | switch_statement | static_statement | return_statement | typedef_statement | remove_statement | sort_statement | CONTINUE_STATEMENT | BREAK_STATEMENT | since_statement | variable_declaration_statement | expression_statement 
    statement = passthru

    # block: "{" ( statement | function_declaration | record_declaration | block )* "}"
    @v_args(meta=True)
    def block(self, children, meta):
        if len(children) > 0:
            return Chunk('{{\n{}\n}}').format(Chunk.join_lines(children)).with_meta(meta)
        else: return Chunk('{}').with_meta(meta)

    # block_or_statement: block | statement
    block_or_statement = passthru

    # file: ( statement | function_declaration | record_declaration | import_statement )+
    def file(self, chlidren):
        return Chunk('{}\n').format(Chunk.join_lines(chlidren, indent=False))

comments = []
l = Lark('''
         COMMENT: "/*" /(.|\\n)*?/ "*/" | "//" /.*/
         %ignore COMMENT
         %import common.WS
         %ignore WS
         ENUMERATED_TYPE: "item" | "location" | "class" | "stat" | "skill" | "effect" | "familiar" | "slot" | "monster" | "element" | "coinmaster" | "phylum" | "thrall"
         ENUMERATED_TYPES: "ints" | "strings" | "items" | "locations" | "classes" | "stats" | "skills" | "effects" | "familiars" | "slots" | "monsters" | "elements" | "coinmasters" | "phylums" | "thralls"
         PRIMITIVE_TYPE: "boolean" | "int" | "float" | "string" | "buffer" | "matcher" | "aggregate"
         BOOLEAN: "true" | "false"
         IDENTIFIER: /[a-zA-Z_][0-9a-zA-Z_]*/
         NUMBER: /[+-]?[0-9]+(\.[0-9]+)?/ | /[+-]?\.[0-9]+/
         STRING_LITERAL: /".*?(?<!\\\\)"/ | /'.*?(?<!\\\\)'/
         TEMPLATE_CHARACTERS: /[^{`]+/
         template_head: "`" TEMPLATE_CHARACTERS? "{"
         template_middle: "}" TEMPLATE_CHARACTERS? "{"
         template_tail: "}" TEMPLATE_CHARACTERS? "`"
         template_substituted: template_head expression ( template_middle expression )* template_tail
         template_pure: "`" TEMPLATE_CHARACTERS? "`"
         template_literal: template_pure | template_substituted
         UNARY_OPERATOR: "--" | "++"
         PREFIX_UNARY_OPERATOR: "-" | "!"
         BINARY_OPERATOR: "==" | "!=" | "<=" | ">=" | "||" | "&&" | "//" | "/*" | "<<" | ">>" | ">>>" | "**" | "+=" | "-=" | "*=" | "/=" | "%=" | "**=" | "&=" | "^=" | "|=" | "<<=" | ">>=" | ">>>" | "..." | "+" | "-" | "*" | "/" | "%" | ">" | "<" | "=" | "&" | "|" | "contains"
         CONTINUE_STATEMENT: "continue" ";"
         BREAK_STATEMENT: "break" ";"
         function_call: IDENTIFIER "(" ( expression ( "," ( expression? ) )* )? ")"
         index_expression: expression "[" expression "]"
         unary_expression: ( PREFIX_UNARY_OPERATOR | UNARY_OPERATOR ) expression
         unary_postfix_expression: expression UNARY_OPERATOR
         binary_expression: expression BINARY_OPERATOR expression
         ternary_expression: expression "?" expression ":" expression
         paren_expression: "(" expression ")"
         field_expression: expression "." IDENTIFIER
         method_expression: expression "." function_call
         call_expression: "call" function_call
         new_expression: "new" function_call
         enum_name: /(\\[[0-9]+\\])?[a-zA-Z0-9-&; '"?,\\\\()\\.*:!\\/í_]+/
         singular_enum_expression: "$" ENUMERATED_TYPE "[" enum_name "]"
         plural_enum_expression: "$" ENUMERATED_TYPES "[" ( enum_name ( "," enum_name )* )? "]"
         enum_expression: singular_enum_expression | plural_enum_expression
         expression: binary_expression | unary_expression | unary_postfix_expression | ternary_expression | enum_expression | method_expression | field_expression | index_expression | paren_expression | function_call | call_expression | new_expression | NUMBER | STRING_LITERAL | template_literal | BOOLEAN | IDENTIFIER
         index_type: PRIMITIVE_TYPE | ENUMERATED_TYPE | /[0-9]+/
         aggregate_type: type "[" index_type ( "," index_type )* "]"
         type: PRIMITIVE_TYPE | ENUMERATED_TYPE | aggregate_type | IDENTIFIER
         return_type: "void" | type
         dict_literal_entry: ( NUMBER | STRING_LITERAL | BOOLEAN | enum_expression ) ":" expression
         dict_literal: "{" dict_literal_entry ( "," dict_literal_entry )* ","? "}"
         array_literal: "{" ( expression ( "," expression )* )? ","? "}"
         remove_statement: "remove" index_expression ";"
         sort_statement: "sort" expression "by" expression ";"
         since_statement: "since" NUMBER ";"
         expression_statement: expression ";"
         if_statement: "if" "(" expression ")" block_or_statement ( "else" block_or_statement )?
         while_statement: "while" "(" expression ")" block_or_statement
         for_statement: "for" IDENTIFIER "from" expression "to" expression ( "by" expression )? block_or_statement
         foreach_statement: "foreach" IDENTIFIER ( "," IDENTIFIER )* "in" expression block_or_statement
         case: ( "case" expression | "default" ) ":" ( block_or_statement )*
         switch_statement: "switch" "(" expression ")" "{" case* "}"
         static_statement: "static" block
         return_statement: "return" expression? ";"
         typedef_statement: "typedef" type IDENTIFIER ";"
         variable_declaration: type IDENTIFIER
         variable_declaration_statement: variable_declaration ( "=" expression | "="? ( dict_literal | array_literal ) )? ";"
         argument_declaration: type IDENTIFIER
         function_declaration: return_type IDENTIFIER "(" ( argument_declaration ( "," argument_declaration )* )? ")" block
         record_declaration: "record"i IDENTIFIER "{" ( variable_declaration ";" )+ "}" ";"
         import_statement: "import" ( STRING_LITERAL | /<.*?(?<!\\\\)>/ )
         statement: if_statement | while_statement | for_statement | foreach_statement | switch_statement | static_statement | return_statement | typedef_statement | remove_statement | sort_statement | CONTINUE_STATEMENT | BREAK_STATEMENT | since_statement | variable_declaration_statement | expression_statement 
         block: "{" ( statement | function_declaration | record_declaration | block )* "}"
         block_or_statement: block | statement
         file: ( statement | function_declaration | record_declaration | import_statement )+
         ''', start='file', propagate_positions=True, lexer_callbacks={ 'variable_declaration': print })

ashref = set(open('ashref.txt').read().splitlines())

for filename in sys.argv[1:]:
    # print(filename)
    tree = l.parse(open(filename).read())
    transformed = Rewriter(visit_tokens=True).transform(tree)
    print(transformed)
    # print(comments)
# tree = l.parse('''
# `{b}v`;
# ''')
# transformed = Rewriter(visit_tokens=True).transform(tree)
# print(transformed)
# print(tree)
