#!/bin/bash
# https://github.com/burtonsamograd/sxc/wiki/How-to-build-an-executable-with-SBCL
sbcl --no-userinit --load xeh.lisp --eval "(sb-ext:save-lisp-and-die \"xeh\" :toplevel 'main :executable t)"
