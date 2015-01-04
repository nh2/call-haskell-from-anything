from ctypes import *
import msgpack

lib = cdll.LoadLibrary("dist/build/call-haskell-from-anything.so/call-haskell-from-anything.so")

lib.hs_init(0, 0)

# Set function return type to string
fun = lib.f1_t_export
fun.restype = c_char_p

# Call function
msg = msgpack.packb([1, 2.23])
resmsg = fun(msg)
res = msgpack.unpackb(resmsg)

print "Haskell said:", res


# Some shortcuts
def make_msgpack_fun(fun):
    fun.restype = c_char_p

    def f(*args):
        return msgpack.unpackb(fun(msgpack.packb(args)))

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
