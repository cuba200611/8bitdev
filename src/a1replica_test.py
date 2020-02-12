from    testmc.m6502 import  Machine, Registers as R
import  pytest


@pytest.fixture
def M():
    M = Machine()
    M.load('.build/obj/src/a1replica.p')
    return M

####################################################################
#   Utility routines

@pytest.mark.parametrize('x, cycles', (
    (0x01,      5), # "no" delay: 5 μs
    (0x02,   1801), # minimum real delay: 1.8 ms
    (0x03,   3597), # next step: 3.6 ms; each additional step adds about 1.8 ms
   #(0x81, 229893), # half total delay available: about 230 ms
))
def test_humdly(M, x, cycles):
    M.call(M.symtab.loopdly, R(x=x))
    assert cycles == M.mpu.processorCycles  # test framework doesn't include RTS

@pytest.mark.skip(reason='Slow test')
def test_humdlymax(M):
    M.call(M.symtab.loopdlymax, R(x=3), maxops=1000000000)
    assert 457987 == M.mpu.processorCycles  # about half a second at 1 MHz