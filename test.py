from ctypes import *
import msgpack

# lib = cdll.LoadLibrary("./pyhaskell.so")
lib = cdll.LoadLibrary("dist/build/pyhaskell.so/pyhaskell.so")

lib.hs_init(0, 0)

fun = lib.f1_hs
fun.restype = c_char_p

# while True:
#     x = fun("asdf" * 1024 * 1024)
#     print x

msg = msgpack.packb([1, 2.23])
# msg = msgpack.packb("sadfasdf")
resmsg = fun(msg)
print msgpack.unpackb(resmsg)

lib.hs_exit()
