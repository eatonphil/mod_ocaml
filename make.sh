#!/bin/sh

export PATH=$PATH:/usr/local/apache/bin
apxs -c mod_ocaml.c
cp mod_ocaml.c /usr/local/apache/libexec
apachectl stop
sleep 1
apachectl start
