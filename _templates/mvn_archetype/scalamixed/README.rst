Scalamixed-archetype
===========================================
.. .rst to .html: rst2html5 foo.rst > foo.html
..                pandoc -s -f rst -t html5 -o foo.html foo.rst

A Maven archetype template for mixed Scala project.

Installation
------------
source code tarball download:
    
        # [aria2c --check-certificate=false | wget --no-check-certificate | curl -kOL]
        
        FETCHCMD='aria2c --check-certificate=false'
        
        $FETCHCMD https://bitbucket.org/thebridge0491/scalamixed/[get | archive]/master.zip

version control repository clone:
        
        git clone https://bitbucket.org/thebridge0491/scalamixed.git

cp pom.xml src/main/resources/ ; mvn install

Usage
-----
		// example
		cd <path> ; mvn archetype:generate [-DinteractiveMode=false] -DarchetypeCatalog=local -DarchetypeGroupId=org.sandbox -DarchetypeArtifactId=scalamixed-archetype -Ddate=2012-08-20 -DgroupId=org.sandbox -Dparent=intro_scala -Dname=util -Dversion=0.1.0 [-DtestFrwk=scalatest -DffiLib=none -Dexecutable=no]

Author/Copyright
----------------
Copyright (c) 2012 by thebridge0491 <thebridge0491-codelab@yahoo.com>


License
-------
Licensed under the Apache-2.0 License. See LICENSE for details.

