import os
import os.path as path

code = []
for i in range(3333):
    code.append(("type Dummy{0}\n"
                 "  = Foo{0} Ting\n"
                 "  | Bar{0} Dummy{0}\n"
                 ).format(i))
out = ''.join(code)
# out += "type alias Ting = Int\n"

if not os.path.exists("gen"):
    os.makedirs("gen")

with open(path.join("gen", "LateResolve.miu"), 'w') as f:
    f.write(out)
