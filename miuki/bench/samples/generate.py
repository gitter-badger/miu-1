import os
import os.path as path
import stat

def make_10k():
    code = []
    for i in range(3333):
        code.append(("type Dummy{0}\n"
                     "  = Foo{0} Thing\n"
                     "  | Bar{0} Dummy{0}\n"
        ).format(i))
    out = ''.join(code)
    # out += "type alias Thing = Int\n"
    return out

files = [("10k.miu", make_10k())]

read_only = stat.S_IRUSR | stat.S_IRGRP | stat.S_IROTH

for (p, s) in files:
    if os.path.isfile(p):
        os.remove(p)
    with open(p, 'w') as f:
        f.write(s)
    os.chmod(p, read_only)
