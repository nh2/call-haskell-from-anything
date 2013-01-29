from ctypes import *
import msgpack

lib = cdll.LoadLibrary("dist/build/pyhaskell.so/pyhaskell.so")

lib.hs_init(0, 0)

# Set function return type to string
fun = lib.f1_hs
fun.restype = c_char_p

# Call function
msg = msgpack.packb([1, 2.23])
resmsg = fun(msg)
res = msgpack.unpackb(resmsg)

print "Haskell said:", res

lib.hs_exit()
