#! /bin/sh
csc -c -static src/main.scm -o main.o
csc -c -static src/server.scm -o server.o
csc -c -static src/libparse.scm -o libparse.o
csc -static -O5 libparse.o main.o -o bin/bib2json
csc -static -O5 libparse.o server.o -o bin/bib2jsonserver
rm *.o
