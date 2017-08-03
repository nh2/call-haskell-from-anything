from ctypes import *
import struct
import msgpack

# For finding where the built .so file is, independent on whether it was built with stack or cabal
import os, sys
def find_file_ending_with(ending_with_str, path='.'):
    for root, dirs, files in os.walk(path):
        for candidate_path in [os.path.join(root, f) for f in files]:
            if candidate_path.endswith(ending_with_str):
                return candidate_path
    raise Exception("Could not find " + ending_with_str + " in " + path)
so_file_path = find_file_ending_with('build/call-haskell-from-anything.so/call-haskell-from-anything.so')


free = cdll.LoadLibrary("libc.so.6").free
lib = cdll.LoadLibrary(so_file_path)

lib.hs_init(0, 0)

# Set function return type to string
fun = lib.f1_t_export
fun.restype = POINTER(c_char)

# Call function
msg = msgpack.packb([1, 2.23])
length_64bits = struct.pack(">q", len(msg)) # big-endian
ptr = fun(length_64bits + msg)
data_length = struct.unpack(">q", ptr[:8])[0]
res = msgpack.unpackb(ptr[8:8+data_length])
free(ptr)

print "Haskell said:", res


# Some shortcuts
def make_msgpack_fun(fun):
    fun.restype = POINTER(c_char)

    def f(*args):
        packed = msgpack.packb(args)
        length_64bits = struct.pack(">q", len(packed)) # big-endian
        ptr = fun(length_64bits + packed)
        data_length = struct.unpack(">q", ptr[:8])[0]
        res = msgpack.unpackb(ptr[8:8+data_length])
        free(ptr)
        return res

    return f


# Now this is the only thing required
fib = make_msgpack_fun(lib.fib_export)

print "Haskell fib:", fib(13)


# def fib(n):
#     if n == 0 or n == 1: return 1
#     return fib(n-1) + fib(n-2)

sum = 0
for x in xrange(100000):
    sum += fib(15)
print sum

lib.hs_exit()
