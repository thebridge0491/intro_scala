Intro_scala-Foreignc
===========================================
.. .rst to .html: rst2html5 foo.rst > foo.html
..                pandoc -s -f rst -t html5 -o foo.html foo.rst

FFI sub-package for Scala Intro examples project.

Installation
------------
source code tarball download:
    
        # [aria2c --check-certificate=false | wget --no-check-certificate | curl -kOL]
        
        FETCHCMD='aria2c --check-certificate=false'
        
        $FETCHCMD https://bitbucket.org/thebridge0491/intro_scala/[get | archive]/master.zip

version control repository clone:
        
        git clone https://bitbucket.org/thebridge0491/intro_scala.git

build example with sbt:
cd <path> ; sbt [-Djava.library.path=$PREFIX/lib] compile [test:run]

sbt publishLocal

build example with rake:
cd <path> ; [sh] ./configure.sh [--prefix=$PREFIX] [--help]

rake main [check]

rake publish

build example with make:
cd <path> ; [sh] ./configure.sh [--prefix=$PREFIX] [--help]

make all [check]

make publish

build example with ant:
cd <path> ; ant [-Djava.library.path=$PREFIX/lib] compile [test]

ant publish

build example with maven:
cd <path> ; mvn [-Djava.library.path=$PREFIX/lib] compile [test]

mvn install

build example with gradle:
cd <path> ; gradle [-Djava.library.path=$PREFIX/lib] assemble [check]

gradle install

Usage
-----
        // PKG_CONFIG='pkg-config --with-path=$PREFIX/lib/pkgconfig'
        
        // $PKG_CONFIG --cflags --libs <ffi-lib>
        
        // java [-Djava.library.path=$PREFIX/lib] ...
        
        import org.sandbox.intro_scala.foreignc.Classic
        
        ...
        
        val res = Classic.fact_i(5)

Author/Copyright
----------------
Copyright (c) 2012 by thebridge0491 <thebridge0491-codelab@yahoo.com>

License
-------
Licensed under the Apache-2.0 License. See LICENSE for details.
