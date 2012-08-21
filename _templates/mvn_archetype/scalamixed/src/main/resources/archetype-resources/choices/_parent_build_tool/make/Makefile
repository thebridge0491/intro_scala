#set( $symbol_pound = '#' )
#set( $symbol_dollar = '$' )
#set( $symbol_escape = '\' )
${symbol_pound} Multi-package project Makefile script.
.POSIX:
help:

${symbol_pound}MAKE = make ${symbol_pound} (GNU make variants: make (Linux) gmake (FreeBSD)

parent = ${parent}
SUBDIRS = common app

.PHONY: all clean compile configure help publish testCompile test
configure: ${symbol_dollar}(SUBDIRS) ${symbol_pound}${symbol_pound} configure [OPTS=""]
	-for dirX in ${symbol_dollar}^ ; do mkdir -p ${symbol_dollar}${symbol_dollar}dirX/target ; \
		(cd ${symbol_dollar}${symbol_dollar}dirX ; sh ./configure.sh ${symbol_dollar}(OPTS)) ; done
help: ${symbol_dollar}(SUBDIRS)
	-for dirX in ${symbol_dollar}^ ; do ${symbol_dollar}(MAKE) -C ${symbol_dollar}${symbol_dollar}dirX ${symbol_dollar}@ ; done
	@echo "${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound} Top-level multiproject: ${symbol_dollar}(parent) ${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}"
	@echo "       ${symbol_dollar}(MAKE) [SUBDIRS="${symbol_dollar}(SUBDIRS)"] configure [OPTS=??]"
	@echo "Usage: ${symbol_dollar}(MAKE) [SUBDIRS="${symbol_dollar}(SUBDIRS)"] [target]"
all compile test testCompile publish: ${symbol_dollar}(SUBDIRS)
	-for dirX in ${symbol_dollar}^ ; do ${symbol_dollar}(MAKE) -C ${symbol_dollar}${symbol_dollar}dirX ${symbol_dollar}@ ; done
clean: ${symbol_dollar}(SUBDIRS)
	-for dirX in ${symbol_dollar}^ ; do ${symbol_dollar}(MAKE) -C ${symbol_dollar}${symbol_dollar}dirX ${symbol_dollar}@ ; done
	-rm -fr core* *~ .*~ target/* *.log */*.log

${symbol_pound}----------------------------------------
FMTS ?= tar.gz
distdir = ${symbol_dollar}(parent)-0.1.0

.PHONY: dist jar javadoc scaladoc checkstyle scalastyle cover report run debug
dist: ${symbol_dollar}(SUBDIRS)
	-mkdir -p target/${symbol_dollar}(distdir) ; cp exclude.lst target/
${symbol_pound}	${symbol_pound}-zip -9 -q --exclude @exclude.lst -r - . | unzip -od target/${symbol_dollar}(distdir) -
	-tar --format=posix --dereference --exclude-from=exclude.lst -cf - . | tar -xpf - -C target/${symbol_dollar}(distdir)
	
	-@for fmt in `echo ${symbol_dollar}(FMTS) | tr ',' ' '` ; do \
		case ${symbol_dollar}${symbol_dollar}fmt in \
			zip) echo "### target/${symbol_dollar}(distdir).zip ###" ; \
				rm -f target/${symbol_dollar}(distdir).zip ; \
				(cd target ; zip -9 -q -r ${symbol_dollar}(distdir).zip ${symbol_dollar}(distdir)) ;; \
			*) tarext=`echo ${symbol_dollar}${symbol_dollar}fmt | grep -e '^tar${symbol_dollar}${symbol_dollar}' -e '^tar.xz${symbol_dollar}${symbol_dollar}' -e '^tar.bz2${symbol_dollar}${symbol_dollar}' || echo tar.gz` ; \
				echo "### target/${symbol_dollar}(distdir).${symbol_dollar}${symbol_dollar}tarext ###" ; \
				rm -f target/${symbol_dollar}(distdir).${symbol_dollar}${symbol_dollar}tarext ; \
				(cd target ; tar --posix -L -caf ${symbol_dollar}(distdir).${symbol_dollar}${symbol_dollar}tarext ${symbol_dollar}(distdir)) ;; \
		esac \
	done
	-@rm -r target/${symbol_dollar}(distdir)
${symbol_pound}	-for dirX in ${symbol_dollar}^ ; do ${symbol_dollar}(MAKE) -C ${symbol_dollar}${symbol_dollar}dirX ${symbol_dollar}@ ; done
jar javadoc scaladoc checkstyle scalastyle cover report: ${symbol_dollar}(SUBDIRS)
	-for dirX in ${symbol_dollar}^ ; do ${symbol_dollar}(MAKE) -C ${symbol_dollar}${symbol_dollar}dirX ${symbol_dollar}@ ; done
run debug: app
	-${symbol_dollar}(MAKE) -C app ${symbol_dollar}@
