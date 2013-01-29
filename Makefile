GHC=ghc
# Call with override, e.g.:
# make GHC_RUNTIME_LINKER_FLAG=$HOME/opt/haskell-7.4/lib/ghc-7.4.2/libHSrts-ghc7.4.2.so
GHC_RUNTIME_LINKER_FLAG=-lHSrts-ghc7.4.2.so

pyhaskell.so: src/FFI/Python.hs
	$(GHC) --make -dynamic -shared -fPIC src/FFI/Python.hs -o pyhaskell.so $(GHC_RUNTIME_LINKER_FLAG)

clean:
	rm -rf pyhaskell.so src/FFI/Python.o src/FFI/Python_stub.h


test: pyhaskell.so
	python test.py
