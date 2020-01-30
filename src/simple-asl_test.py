from    testmc.m6502 import  Machine, Registers as R
import  pytest


@pytest.fixture
def M():
    M = Machine()

    #   XXX Not the best way to find this file: duplicates definition
    #   of $buildir in Test and dependent on CWD.
    M.load('.build/obj/src/simple-asl.p')

    #   Confirm correct file is loaded
    ident = M.symtab.ident
    assert 0x240 == ident
    ident_str = "simple-asl.a65"
    assert ident_str == M.str(ident, len(ident_str))

    return M

def test_brk(M):
    S = M.symtab
    assert 0x00 == M.byte(S.testbrk)
    assert 0xFF == M.byte(S.testbrk+1), \
        'redefined BRK should not add EA after 00'

    assert 0x00 == M.byte(S.testbrkarg)
    assert 0x38 == M.byte(S.testbrkarg+1)
    assert 0xFE == M.byte(S.testbrkarg+2)

def test_ds_db_dw(M):
    S = M.symtab
    start = S.defalloctest
    assert S.dstest0            == start
    assert S.dstest1            == start+3
    assert S.dbtest             == start+4
    assert b'\x00abc\xFF\xFF'   == bytes(M.bytes(S.dbtest, 6))
    assert S.dwtest             == start+10
    assert 0xABCD               == M.word(S.dwtest)
    assert (0xCD, 0xAB)         == (M.byte(S.dwtest), M.byte(S.dwtest+1))

def test_zds(M):
    S = M.symtab

    #   The initial value of __ZDS_loc at the zdstest0 label.
    #   This value $10 assumes that zdstest1 is the first ZDS in this assembly.
    zloc = 0x10

    assert 0xFF        < S.zdstest0
    assert 0xFF       >= S.zdstest1
    assert zloc       == S.zdstest1
    assert 0xFF       >= S.zdstest2
    assert zloc+3     == S.zdstest2
    assert 0xFF        < S.zdstest3
    assert S.zdstest3 == S.zdstest0 + 1     # Original location ctr. preserved

@pytest.mark.parametrize('input, expected', (
    (0x0000, 0x0001),
    (0x1234, 0x1235),
    (0x00FE, 0x00FF),
    (0x00FF, 0x0100),
    (0xFF00, 0xFF01),
    (0xFFFF, 0x0000),
))
def test_incw(M, input, expected):
    incw = M.symtab.incwtest
    data = M.symtab.incwdata
    r    = R(a=0x12, x=0x34, y=0x56)

    M.depword(data, input)
    M.call(incw, r)
    assert r == M.regs
    assert expected == M.word(data)

def test_addxy(M):
    S = M.symtab
    M.call(S.addxy, R(x=0x2A, y=0x33, C=1))
    expected = 0x2A + 0x33
    assert expected == M.byte(S.xybuf)
    assert R(a=expected, x=0x2A, y=0x33) == M.regs

@pytest.mark.parametrize('len', (1, 2, 16, 255))    # len must be at least 1
def test_fill(M, len):
    S = M.symtab
    base = 0x1234       # Fill does not include base address

    guard = [0xFF] * 2
    M.deposit(base-2, [0xFF]*260)

    #   There's also a temporary symbol in here but we currently have
    #   neither a reliable way to access it or any good use for it.
    M.depword(S.fillbase, base)
    M.call(S.fill, R(y=len))
    assert guard + [0xFF] + [0]*len + guard == M.bytes(base-2, len+5)

def test_negoff(M):
    S = M.symtab

    #   Show what negoff() calculates.
    M.call(S.negoffcalc)
    print('no_data=${:04X} negoff=${:04X}'
        .format(S.no_data, M.word(S.no_dbgaddr)))

    #   Confirm we have correct data in memory.
    assert [0xE0, b'0a5g8s', 0xE1] \
        == [ M.byte(S.no_data-1),           # guard
             bytes(M.bytes(S.no_data, 6)),  # string
             M.byte(S.no_data+6),           # guard
           ]

    #   Run the demo and confirm it worked.
    M.call(S.negoffdemo, trace=True)
    assert [0xE0, b'1b6h9t', 0xE1] \
        == [ M.byte(S.no_data-1),           # guard
             bytes(M.bytes(S.no_data, 6)),  # string
             M.byte(S.no_data+6),           # guard
           ]
    assert R(x=0) == M.regs

def test_chromatix_id(M):
    M.call(M.symtab.chromatix_id)
    expa = ord('S')             # 65C02, this emulator is, apparently
    assert R(a=expa) == M.regs
