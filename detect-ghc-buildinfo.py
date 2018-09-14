import argparse
import subprocess


# Get the compiler binary from cabal.
# Enables use of cabal's -w/--with-compiler option.
parser = argparse.ArgumentParser()
parser.add_argument(
  '--with-compiler', default="ghc", type=str,
  help='the compiler passed to cabal configure'
)
args, _unknown = parser.parse_known_args()  # ignore unknown arguments from cabal

ghc_binary = args.with_compiler


print("Determining GHC version:")
ghc_version = subprocess.check_output([ghc_binary, "--numeric-version"]).strip()
print(ghc_version)


with open("call-haskell-from-anything.buildinfo", "w") as f:
  f.writelines('\n'.join([
    "executable: call-haskell-from-anything.so",
    "extra-libraries: HSrts-ghc{0}".format(ghc_version),
    "",  # newline at end of file
  ]))
