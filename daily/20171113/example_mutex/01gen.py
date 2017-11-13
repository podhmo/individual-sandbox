from prestring.go import Module
import sys
m = Module()
N = int(sys.argv[1])

m.package('main')
with m.import_group() as im:
    im('sync')
    im('fmt')
    im('math/rand')

with m.type_('S struct'):
    for i in range(N):
        m.stmt(f'Value{i} int')

with m.func('main'):
    m.stmt('var wg sync.WaitGroup')
    m.stmt("var m sync.Mutex")
    m.stmt('s := S{}')
    for i in range(N):
        m.stmt('wg.Add(1)')
        m.stmt('go func(){')
        with m.scope():
            m.stmt("m.Lock()")
            m.stmt("defer m.Unlock()")
            m.stmt(f's.Value{i} = rand.Int()')
            m.stmt('wg.Done()')
        m.unnewline()
        m.stmt('}()')
    m.stmt("wg.Wait()")
    m.stmt('fmt.Printf("%#+v\\n", s)')
print(m)
