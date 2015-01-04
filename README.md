call-haskell-from-anything
==========================

[![Build Status](https://travis-ci.org/nh2/call-haskell-from-anything.png)](https://travis-ci.org/nh2/call-haskell-from-anything)

Call Haskell functions from any programming language via serialization and dynamic libraries.

[Skip the philosophy, jump right to the code!](#usage)


I just want to call that function
---------------------------------

```
Want to call Haskell from Python?
Want to call Haskell from Ruby?
Want to call Haskell from C?
Want to call Haskell from Node.js?
Want to call Haskell from C#?
Want to call Haskell from Java?
Want to call Haskell from browsers?
```

Yes, Haskell can do that.

Using the Foreign Function Interface (FFI) you can expose Haskell functions at the C level.

But damn, it's so hard!

You have two high-level languages here (Haskell and X), but even though you "just want to call that function", you have to think about and write low-level system code on both sides.
Going via C is painful: An interface that does not even support the idea of *many of something* is not very supportive (no, C doesn't even have *arrays*, it only has pointers to the start of something).

What we really want for most cases:
* a slightly higher level, intuitive interface
* as invisible as possible
* just calling that function.

```
Want to call Haskell from ... anything?
```


The simplest FFI: Serialization
-------------------------------

> In the end, the C calling convention is just another wire format:
> Data is to be shipped from one function to another.

So we could just as well use a wire format that is not as uncomfortable as the C FFI.

*Any* serialization library does that for us, and most of them (e.g. JSON) are simpler to reason about and manage than raw memory in C.

*call-haskell-from-anything* implements FFI function calls where function arguments and return value are serialized using [MessagePack](http://msgpack.org).
Any function is then exported via the standard FFI as a raw bytes (`CString -> IO CString`) interface.


Usage
-----

*call-haskell-from-anything* allows you to write a function, say:

```haskell
chooseMax :: [Int] -> Int
chooseMax xs = ...
```

Add this:

```haskell
foreign export ccall chooseMax_export :: CString -> IO CString
chooseMax_export = export . returnId2 $ chooseMax
```

and compile it into a shared library (`.so` or `.dll`).
You can now call it from any language that supports MessagePack, e.g. Python:

```python
chooseMax = wrap_into_msgpack(cdll.LoadLibrary('mylib.so').chooseMax_export)

print chooseMax([1,5,3])
```

--

In detail, it will transform your functions of type

```haskell
f :: a -> b -> ... -> r
```

to an equivalent (it is actually a type-level list) of

```haskell
f' :: (a, b, ...) -> r
```

so that the function *input* (arguments) can be easily de-serialized.

The only restriction for pure functions is that they must be lifted to return a result in the `Identity` monad; the convenience functions `returnId2`, `returnId3` and so on do this for you.

The `wrap_into_msgpack` function used above sets the return type of the foreign function to raw bytes and wraps arguments and return value into MessagePack:

```python
def wrap_into_msgpack(foreign_fun):
    foreign_fun.restype = c_char_p

    def wrapped_fun(*args):
        return msgpack.unpackb(foreign_fun(msgpack.packb(args)))

    return wrapped_fun
```


A full example
--------------

You can run the stock example in this repository:

```bash
sudo apt-get install python-msgpack  # or equivalent for your system
# Important: All used libraries have to be installed with --enable-shared,
# so better use a cabal sandbox:
cabal sandbox init
cabal install --only-dependencies --enable-shared -j8
cabal configure --enable-shared

# If the above doesn't work, you may have to adjust name of the shared
# library for the Haskell RTS `libHSrts-ghc*.*.*.so` in the
# `detect-ghc-buildinfo` file, depending on how the .so file is called
# in your OS.
# If it is just in a different location, pass
#   --extra-lib-dirs=/path/to/ghc/lib/ghc-*.*.*/rts-1.0/
# to `cabal configure`.

cabal build
python test.py  # If this works, you're all fine!
```

**TODO**: Detail some of the code here.

* Show needed imports and language extensions, link to example file.
* Link to example Python/Ruby/C code loading the dynamic library.


FAQ
---

### Is *call-haskell-from-anything* an RPC framework?

No. RPC means *Remote Procedure Call*, and nothing in *call-haskell-from-anything* assumes to be remote.

Calls are blocking as you would expect from standard C libraries.


### Are there restrictions on what arguments functions can take?

Yes: all arguments and the return value must be serializable.

This means you cannot pass around pointers or callback functions; you have to use the C style FFI or an RPC mechanism for that.


### Why is MsgPack used for serialization?

Because it is simple, available (there are implementations for most programming languages, and writing new ones is easy due to its simplicity), supports dumb binary (passing around arbitrary custom data does not require to think about encoding), and fast (in many implementations).

However, *call-haskell-from-anything* is not fixed to use only MsgPack as wire-format; anything that can conveniently encode lists/arrays is suitable (`FFI.Anything.TypeUncurry.Msgpack` is the only implementation so far, though).


### I cant get the dependencies to install with GHC >= 7.8

The `msgpack` package's dependency bounds haven't been updated for 7.8 as of writing.
Try installing the dependencies with

```bash
cabal install --only-dependencies --enable-shared -j8 --allow-newer=text,attoparsec,template-haskell
```


### How fast are serialized FFI calls? What is the trade-off compared to a C style FFI?

Calls from one programming language into another are usually slower than calls inside the programming language, so it does make sense to check if a foreign call is worth it.

In some preliminary cPython 2.7 benchmark using functions that take a single `Int` and return a single `Int` (e.g. the *+1* function), a foreign call using MsgPack serialization takes around 15 times longer than an in-Python function call (on the tested Core i5 machine, 1M calls took 15s, in pure Python they took 1s). However, as soon as you perform a somewhat expensive computation, the call into native Haskell code becomes worth it (take for example a naive recursive `fibonacci` implementation for 100000 calls of `fib(15)`; in-Python: 90s, with *call-haskell-from-anything*: 4.5s).

In comparison to a C style FFI to an immediately returning `Int -> Int` function, the overhead of a serializing function call is around 6 times higher, and, as usual, becomes insignificant as soon as the function does something.

More detailed benchmarks are planned, and **contributions are welcome**.
