#set( $symbol_pound = '#' )
#set( $symbol_dollar = '$' )
#set( $symbol_escape = '\' )
${symbol_pound} ${parent.substring(0,1).toUpperCase()}${parent.substring(1)}-${name.substring(0,1).toUpperCase()}${name.substring(1)}
<!-- .md to .html: markdown foo.md > foo.html
                   pandoc -s -f markdown_strict -t html5 -o foo.html foo.md -->

${description}

${symbol_pound}${symbol_pound} Installation
source code tarball download:
    
        ${symbol_pound} [aria2c --check-certificate=false | wget --no-check-certificate | curl -kOL]
        FETCHCMD='aria2c --check-certificate=false'
        ${symbol_dollar}FETCHCMD https://${repohost}/${repoacct}/${parent}/[get | archive]/master.zip

version control repository clone:
        
        git clone https://${repohost}/${repoacct}/${parent}.git


#{if}("sbt" == ${buildTool})
build example with sbt self-install:
cd <path> ; ./sbtw [-Djava.library.path=${symbol_dollar}PREFIX/lib] compile [test:run]
./sbtw publishLocal
#{elseif}("rake" == ${buildTool})
build example with rake:
cd <path> ; [sh] ./configure.sh [--prefix=${symbol_dollar}PREFIX] [--help]
rake main [check]
rake publish
#{elseif}("make" == ${buildTool})
build example with make:
cd <path> ; [sh] ./configure.sh [--prefix=${symbol_dollar}PREFIX] [--help]
make all [check]
make publish
#{elseif}("ant" == ${buildTool})
build example with ant:
cd <path> ; ant [-Djava.library.path=${symbol_dollar}PREFIX/lib] compile [test]
ant publish
#{elseif}("maven" == ${buildTool})
build example with maven wrapper:
cd <path> ; ./mvnw [-Djava.library.path=${symbol_dollar}PREFIX/lib] compile [test]
./mvnw install
#{else}
build example with gradle wrapper:
cd <path> ; ./gradlew [-Djava.library.path=${symbol_dollar}PREFIX/lib] assemble [check]
./gradlew install
#{end}

${symbol_pound}${symbol_pound} Usage
#{if}("yes" == ${executable})
        [env RSRC_PATH=<path>/resources] java -jar ${parent}-${name}-<version>.jar [-h]
#{else}
        // PKG_CONFIG='pkg-config --with-path=${symbol_dollar}PREFIX/lib/pkgconfig'
        // ${symbol_dollar}PKG_CONFIG --cflags --libs <ffi-lib>
        // java [-Djava.library.path=${symbol_dollar}PREFIX/lib] ...
        
        import ${package}.Library
        ...
        val (arr1, arr2) = (Array[Int](0, 1, 2), Array[Int](10, 20, 30))
        val nested_arr = Library.cartesian_prod(arr1, arr2)
#{end}

${symbol_pound}${symbol_pound} Author/Copyright
Copyright (c) ${date.split("-")[0]} by ${author} <${email}>


${symbol_pound}${symbol_pound} License
Licensed under the ${license} License. See LICENSE for details.

