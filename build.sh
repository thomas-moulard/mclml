#! /bin/sh

test -x ./_build/sanitize.sh && ./_build/sanitize.sh;
ocamlbuild -lib graphics -I src demo/demo.native
