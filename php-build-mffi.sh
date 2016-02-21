#!/bin/bash

which -s brew
if [[ $? != 0 ]] ; then
    # Install Homebrew
    echo "Homebrew not found, if you are on a mac, please install Homebrew first"
    exit 0;
fi

brew install libffi
git clone https://github.com/mgdm/MFFI
cd MFFI
phpize
LIBFFI_PATH=$(brew --prefix libffi)
LDFLAGS=" -L${LIBFFI_PATH}/lib" PKG_CONFIG_PATH="$PKG_CONFIG_PATH:${LIBFFI_PATH}/lib/pkgconfig" ./configure
make
make install

echo "Copy the path to the PHP extension as shown above."
echo "Then, open up php.ini and add the following lines at the bottom:"
echo ""
echo "extension_dir = \"/usr/local/Cellar/.........../no-debug-non-zts-etc\""
echo "extension=mffi.so"
echo ""

echo "Correct installation can be verified with: "

echo "$ php -r 'phpinfo();' | grep -i \"mffi => enabled\""
echo "  mffi => enabled"
echo ""
