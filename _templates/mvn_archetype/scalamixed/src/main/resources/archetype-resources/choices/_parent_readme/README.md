#set( $symbol_pound = '#' )
#set( $symbol_dollar = '$' )
#set( $symbol_escape = '\' )
${symbol_pound} ${parent.substring(0,1).toUpperCase()}${parent.substring(1)}
<!-- .md to .html: markdown foo.md > foo.html
                   pandoc -s -f markdown_strict -t html5 -o foo.html foo.md -->

${parentdescription}

${symbol_pound}${symbol_pound} Installation
source code tarball download:
    
        ${symbol_pound} [aria2c --check-certificate=false | wget --no-check-certificate | curl -kOL]
        FETCHCMD='aria2c --check-certificate=false'
        ${symbol_dollar}FETCHCMD https://${repohost}/${repoacct}/${parent}/[get | archive]/master.zip

version control repository clone:
        
        git clone https://${repohost}/${repoacct}/${parent}.git

${symbol_pound}${symbol_pound} Author/Copyright
Author: ${author} <${email}>

see sub-package's Author/Copyright

${symbol_pound}${symbol_pound} License
see sub-package's License
