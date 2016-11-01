#!/usr/bin/env python3

from random import choice
import string

TERM, TYPE, KIND = 0, 1, 2
SORT = [TERM, TYPE, KIND]
IDENT = -1
UIDENT = -2
NAMES = ["Arith", "Untyped", "FullUntyped", "TyArith", "SimpleBool",
         "FullSimple", "Bot", "FullRef", "FullError", "RcdSubBot", "FullSub",
         "FullEquiRec", "FullIsoRec", "EquiRec", "Recon", "FullRecon",
         "FullPoly", "FullOmega"]


class NonTerminal:
    def __init__(self, template):
        self.template = template

    def gen(self, depth, g):
        for i in range(0, len(self.template)):
            if self.template[i] in SORT:
                d = choice(list(range(0, depth)))
                self.template[i] = g.gen(self.template[i], d, g)
            elif self.template[i] == IDENT:
                self.template[i] = choice(string.ascii_lowercase[:5])
            elif self.template[i] == UIDENT:
                self.template[i] = choice(string.ascii_uppercase[:5])
        return '(' + " ".join(self.template) + ')'


class Generator:
    def __init__(self, terminals, non_terminals):
        self.terminals = terminals
        self.non_terminals = non_terminals

    def __add__(self, other):
        terminals, non_terminals = self.terminals, self.non_terminals
        for i in SORT:
            terminals[i] += other.terminals[i]
            non_terminals[i] += other.non_terminals[i]
        return Generator(terminals, non_terminals)

    def gen(self, typ, depth, g):
        if depth < 1 or len(self.non_terminals[typ]) == 0:
            return choice(self.terminals[typ])
        else:
            nt = choice(self.non_terminals[typ])
            return nt.gen(depth, g)


class EGenerator(Generator):
    def __init__(self, terminals, nt_templates):
        non_terminals = [NonTerminal(x) for x in nt_templates]
        super().__init__([terminals, [], []], [non_terminals, [], []])


def gBool():
    ts = ['true', 'false']
    nts = [['if', TERM, 'then', TERM, 'else', TERM]]
    return EGenerator(ts, nts)


def gNat():
    ts = ['0', '1', '2', '3', '4']
    nts = [
        ['succ', TERM],
        ['pred', TERM],
        ['iszero', TERM]
    ]
    return EGenerator(ts, nts)


def gArith():
    return gBool() + gNat()


def gVarApp():
    ts = list(string.ascii_lowercase[:5])
    nts = [[TERM, TERM]]
    return EGenerator(ts, nts)


def gUntypedAbs():
    nts = []
    nts.append(['\\', IDENT, '.', TERM])
    return EGenerator([], nts)


def gUntyped():
    return gUntypedAbs() + gVarApp()


def gRecord():
    nts = [
        ['{', IDENT, '=', TERM, ',', IDENT, '=', TERM, '}'],
        [TERM, '.', IDENT]
    ]
    return EGenerator([], nts)


def gLet():
    nts = [['let', IDENT, '=', TERM, 'in', TERM]]
    return EGenerator([], nts)


def gFloatString():
    # TODO
    ts = ['\"apple\"', '\"boy\"', '\"cat\"', '\"dog\"']
    return EGenerator(ts, [])


def gFullUntyped():
    return gArith() + gUntyped() + gRecord() + gFloatString() + gLet()


class ETGenerator(Generator):
    def __init__(self, term_ts, type_ts, term_nt_templates, type_nt_templates):
        term_nts = [NonTerminal(x) for x in term_nt_templates]
        type_nts = [NonTerminal(x) for x in type_nt_templates]
        super().__init__([term_ts, type_ts, []], [term_nts, type_nts, []])


def gTypedBool():
    tmp = ETGenerator([], ['Bool'], [], [])
    return tmp + gBool()


def gTypedNat():
    tmp = ETGenerator([], ['Nat'], [], [])
    return tmp + gNat()


def gTyArith():
    return gTypedBool() + gTypedNat()


def gTyped():
    term_nts = [['\\', IDENT, ':', TYPE, '.', TERM]]
    type_nts = [[TYPE, '->', TYPE]]
    tmp = ETGenerator([], [], term_nts, type_nts)
    return tmp + gVarApp()


def gSimpleBool():
    return gTyped() + gTypedBool()


def gTypedRecord():
    type_nts = [['{', IDENT, ':', TYPE, ',', IDENT, ':', TYPE, '}']]
    tmp = ETGenerator([], [], [], type_nts)
    return tmp + gRecord()


def gTypeVar():
    return ETGenerator([], list(string.ascii_uppercase[:5]), [], [])


def gVariant():
    term_nts = [
        ['<', IDENT, '=', TERM, '>', 'as', TYPE],
        ['case', TERM, 'of',
         '<', IDENT, '=', IDENT, '>', '=>', TERM, '|',
         '<', IDENT, '=', IDENT, '>', '=>', TERM]
    ]
    type_nts = [['<', IDENT, ':', TYPE, ',', IDENT, ':', TYPE, '>']]
    return ETGenerator([], [], term_nts, type_nts)


def gExtension():
    term_ts = ['unit']
    type_ts = ['Unit', 'String', 'Float']
    term_nts = [
        [TERM, 'as', TYPE],
        ['fix', TERM],
        ['inert', '[', TYPE, ']']
    ]
    return ETGenerator(term_ts, type_ts, term_nts, [])


def gSimple():
    return gTyArith() + gTyped() + gFloatString() + gLet() + gTypedRecord() + gExtension() + gTypeVar()


def gFullSimple():
    return gSimple() + gVariant()


def gTop():
    return ETGenerator([], ['Top'], [], [])


def gTopBot():
    return ETGenerator([], ['Bot'], [], []) + gTop()


def gBot():
    return gTyped() + gTopBot()


def gRef():
    term_nts = [
        ['ref', TERM],
        ['!', TERM],
        [TERM, ':=', TERM]
    ]
    type_nts = [['Ref', TYPE]]
    return ETGenerator([], [], term_nts, type_nts)


def gSourceSink():
    type_nts = [
        ['Source', TYPE],
        ['Sink', TYPE]
    ]
    return ETGenerator([], [], [], type_nts)


def gFullRef():
    return gFullSimple() + gTopBot() + gRef() + gSourceSink()


def gError():
    return ETGenerator(['error'], [], [['try', TERM, 'with', TERM]], [])


def gFullError():
    return gBot() + gTypedBool() + gError() + gTypeVar()


def gRcdSubBot():
    return gBot() + gTypedRecord()


def gFullSub():
    return gSimple() + gTop()


def gRecType():
    return ETGenerator([], [], [], [['Rec', UIDENT, '.', TYPE]])


def gFullEquiRec():
    return gFullSimple() + gRecType()


def gFold():
    term_nts = [
        ['fold', '[', TYPE, ']', TERM],
        ['unfold', '[', TYPE, ']', TERM]
    ]
    return ETGenerator([], [], term_nts, [])


def gFullIsoRec():
    return gFullEquiRec() + gFold()


def gEquiRec():
    return gTyped() + gRecType() + gTypeVar()


def gRecon():
    return gTyped() + gTyArith() + gTypeVar()


def gFullRecon():
    return gRecon() + gLet()


def gPack():
    term_nts = [
        ['{', '*', TYPE, ',', TERM, '}', 'as', TYPE],
        ['let', '{', UIDENT, ',', IDENT, '}', '=', TERM, 'in', TERM]
    ]
    return ETGenerator([], [], term_nts, [])


def gPoly():
    term_nts = [
        ['\\', UIDENT, '.', TERM],
        [TERM, '[', TYPE, ']']
    ]
    type_nts = [
        ['All', UIDENT, '.', TYPE],
        ['Some', '{', UIDENT, ',', TYPE, '}']
    ]
    return ETGenerator([], [], term_nts, type_nts)


def gFullPoly():
    return gSimple() + gPoly() + gPack()


def gOmega():
    terminals = [[], [], ['Star']]
    term_nts = [
        ['\\', UIDENT, ':', KIND, '.', TERM],
        [TERM, '[', TYPE, ']']
    ]
    type_nts = [
        ['All', UIDENT, ':', KIND, '.', TYPE],
        ['{', 'Some', UIDENT, ':', KIND, ',', TYPE, '}'],
        ['\\', UIDENT, ':', KIND, '.', TYPE],
        [TYPE, TYPE]
    ]
    kind_nts = [[KIND, '=>', KIND]]
    non_terminals = [
        [NonTerminal(x) for x in term_nts],
        [NonTerminal(x) for x in type_nts],
        [NonTerminal(x) for x in kind_nts]
    ]
    return Generator(terminals, non_terminals)


def gFullOmega():
    return gSimple() + gRef() + gPack() + gOmega()


def gen_by_id(id, times, depth):
    gens = [
        gArith(),
        gUntyped(),
        gFullUntyped(),
        gTyArith(),
        gSimpleBool(),
        gFullSimple(),
        gBot(),
        gFullRef(),
        gFullError(),
        gRcdSubBot(),
        gFullSub(),
        gFullEquiRec(),
        gFullIsoRec(),
        gEquiRec(),
        gRecon(),
        gFullRecon(),
        gFullPoly(),
        gFullOmega()
    ]
    assert(len(gens) == len(NAMES))
    ret = []
    g = gens[id]
    for i in range(0, times):
        ret.append(g.gen(TERM, depth, g))
    return ret


def main():
    for i in range(0, len(NAMES)):
        print(NAMES[i])
        print(gen_by_id(i, 1, 10))
        print('')


if __name__ == '__main__':
    main()
