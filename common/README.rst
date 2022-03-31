Intro_scala-Util
===========================================
.. .rst to .html: rst2html5 foo.rst > foo.html
..                pandoc -s -f rst -t html5 -o foo.html foo.rst

Utilities sub-package for Scala Intro examples project.

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

build example with maven wrapper:
cd <path> ; ./mvnw [-Djava.library.path=$PREFIX/lib] compile [test]

./mvnw install

build example with gradle wrapper:
cd <path> ; ./gradlew [-Djava.library.path=$PREFIX/lib] assemble [check]

./gradlew install

Usage
-----
        // PKG_CONFIG='pkg-config --with-path=$PREFIX/lib/pkgconfig'
        
        // $PKG_CONFIG --cflags --libs <ffi-lib>
        
        // java [-Djava.library.path=$PREFIX/lib] ...
        
        import org.sandbox.intro_scala.util.Library
        
        ...
        
        val (arr1, arr2) = (Array[Int](0, 1, 2), Array[Int](10, 20, 30))
        
        val nested_arr = Library.cartesian_prod(arr1, arr2)

Author/Copyright
----------------
Copyright (c) 2012 by thebridge0491 <thebridge0491-codelab@yahoo.com>

License
-------
Licensed under the Apache-2.0 License. See LICENSE for details.
