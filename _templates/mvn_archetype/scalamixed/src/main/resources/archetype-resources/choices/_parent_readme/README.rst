#set( $symbol_pound = '#' )
#set( $symbol_dollar = '$' )
#set( $symbol_escape = '\' )
${parent.substring(0,1).toUpperCase()}${parent.substring(1)}
===========================================
.. .rst to .html: rst2html5 foo.rst > foo.html
..                pandoc -s -f rst -t html5 -o foo.html foo.rst

${parentdescription}

Installation
------------
source code tarball download:
    
        ${symbol_pound} [aria2c --check-certificate=false | wget --no-check-certificate | curl -kOL]
        
        FETCHCMD='aria2c --check-certificate=false'
        
        ${symbol_dollar}FETCHCMD https://${repohost}/${repoacct}/${parent}/[get | archive]/master.zip

version control repository clone:
        
        git clone https://${repohost}/${repoacct}/${parent}.git

Author/Copyright
----------------
Author: ${author} <${email}>
    
see sub-package's Author/Copyright

License
-------
see sub-package's License
