# Targets Makefile script.
#----------------------------------------
# Common automatic variables legend (GNU make: make (Linux) gmake (FreeBSD)):
# $* - basename (cur target)  $^ - name(s) (all depns)  $< - name (1st depn)
# $@ - name (cur target)      $% - archive member name  $? - changed depns

ifeq ($(shell sh -c 'uname -s 2>/dev/null || echo not'),Linux)
JAVA_LIB = /usr/share/java
SCALA_HOME = /usr/share/scala
GROOVY_HOME = /usr/share/groovy
else
JAVA_LIB = /usr/local/share/java/classes
SCALA_HOME = /usr/local/share/scala
GROOVY_HOME = /usr/local/share/java/groovy
endif

ifdef DEBUG
JAVAC_OPTS := $(JAVAC_OPTS) -g
SCALAC_OPTS := $(SCALAC_OPTS) -g:vars
GROOVYC_OPTS := $(GROOVYC_OPTS) -F-g
else
SCALAC_OPTS := $(SCALAC_OPTS) -opt:l:method -opt:l:inline
GROOVYC_OPTS := $(GROOVYC_OPTS)
endif

java_lib_path := $(java_lib_path):$(LD_LIBRARY_PATH)
SCALA_COMPAT ?= 2.13
SCALA_LIB = $(SCALA_HOME)/lib
GROOVY_LIB = $(GROOVY_HOME)/lib
JAVAC_OPTS := $(JAVAC_OPTS) -Xlint:all -deprecation # -target 11 -source 11
SCALAC_OPTS := $(SCALAC_OPTS) -Xlint -deprecation -feature -unchecked # -target:jvm-11
GROOVYC_OPTS := $(GROOVYC_OPTS) -j
JAVA_ARGS = -esa -ea -Xmx1024m -Xms16m -Xss16m
SCALA_ARGS = -J-esa -J-ea -J-Xmx1024m -J-Xms16m -J-Xss16m

classes_dir = target/classes/main
classes_test_dir = target/classes/test

ifeq ($(shell test -e target/classpath_compile.txt && echo -n yes),yes)
classpath_compile = $(shell cat target/classpath_compile.txt)
endif
ifeq ($(shell test -e target/classpath_runtime.txt && echo -n yes),yes)
classpath_runtime = $(shell cat target/classpath_runtime.txt)
endif
ifeq ($(shell test -e target/classpath_test.txt && echo -n yes),yes)
classpath_test = $(shell cat target/classpath_test.txt)
endif

COMPILE.java = javac $(JAVAC_OPTS)
COMPILE.scala = scalac $(SCALAC_OPTS)	# scalac | fsc
COMPILE.groovy = groovyc $(GROOVYC_OPTS)

$(dist_test_jar) : $(tests_java) $(tests_scala) $(tests_groovy)
	-rm -fr $@ # ; -fsc -reset
	-echo -cp $(classpath_test):$(classes_dir):$(classes_test_dir) \
		-d $(classes_test_dir) > target/.argfile
	-$(COMPILE.scala) $(tests_java) $(tests_scala) @target/.argfile
#	-$(COMPILE.groovy) `cat target/.argfile` $(tests_java) $(tests_groovy)
	-$(COMPILE.java) $(tests_java) @target/.argfile
	-echo 'Class-Path: ' > target/test_manifest.mf
	-@for strX in `echo $(classpath_test) | tr : ' ' | \
			sed 's/\([^ ]*\/\)\([^/]*\.jar\)/lib\/\2/g'` $(proj)-$(version).jar ; do \
		echo "  $${strX}" >> target/test_manifest.mf ; \
	done
	-jar -cfme $@ target/test_manifest.mf $(groupid).$(parent).$(pkg).Ts_Main \
		-C $(classes_test_dir) .
$(dist_jar) : $(src_java) $(src_scala) $(src_groovy)
	-rm -fr $@ # ; -fsc -reset
	-echo -cp $(classpath_compile):$(classes_dir) -d $(classes_dir) > \
		target/.argfile
	-$(COMPILE.scala) $(src_java) $(src_scala) @target/.argfile
#	-$(COMPILE.groovy) `cat target/.argfile` $(src_java) $(src_groovy)
	-$(COMPILE.java) $(src_java) @target/.argfile
	-echo 'Class-Path: ' > target/manifest.mf
	-@for strX in `echo $(classpath_runtime) | tr : ' ' | \
			sed 's/\([^ ]*\/\)\([^/]*\.jar\)/lib\/\2/g'` ; do \
		echo "  $${strX}" >> target/manifest.mf ; \
	done
	-jar -cfm $@ target/manifest.mf -C $(classes_dir) .
	-if [ ! "" = "$(main_class)" ] ; then \
		jar -ufe $@ $(main_class) -C src/main/resources . ; \
	fi


FMTS ?= tar.gz,zip
distdir = $(proj)-$(version)

JAVADOC_OPTS := $(JAVADOC_OPTS) -use -private -version -author
GROOVYDOC_OPTS := $(GROOVYDOC_OPTS) -author -private
SCALADOC_OPTS := $(SCALADOC_OPTS) -author

target/$(distdir) :
	-@mkdir -p target/$(distdir) ; cp -f exclude.lst target/
#	#-zip -9 -q --exclude @exclude.lst -r - . | unzip -od target/$(distdir) -
	-tar --format=posix --dereference --exclude-from=exclude.lst -cf - . | tar -xpf - -C target/$(distdir)

.PHONY: help clean test jar javadoc scaladoc checkstyle scalastyle cover report
help: ## help
	@echo "##### subproject: $(proj) #####"
	@echo "Usage: $(MAKE) [target] -- some valid targets:"
#	-@for fileX in $(MAKEFILE_LIST) `if [ -z "$(MAKEFILE_LIST)" ] ; then echo Makefile Makefile-*.mk ; fi` ; do \
#		grep -ve '^[A-Z]' $$fileX | awk '/^[^.%][-A-Za-z0-9_]+[ ]*:.*$$/ { print "...", substr($$1, 1, length($$1)) }' | sort ; \
#	done
	-@for fileX in $(MAKEFILE_LIST) `if [ -z "$(MAKEFILE_LIST)" ] ; then echo Makefile Makefile-*.mk ; fi` ; do \
		grep -E '^[ a-zA-Z_-]+:.*?## .*$$' $$fileX | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "%-25s%s\n", $$1, $$2}' ; \
	done
clean: ## clean build artifacts
	-rm -rf core* target/* target/.??* *.log .coverage
test: testCompile ## run test [TOPTS=""]
#	export [DY]LD_LIBRARY_PATH=. # ([da|ba|z]sh Linux)
#	setenv [DY]LD_LIBRARY_PATH . # (tcsh FreeBSD)
	-java -Djava.library.path=$(java_lib_path) $(JAVA_ARGS) -jar $(dist_test_jar) $(TOPTS)
#dist | target/$(distdir): ## [FMTS="tar.gz,zip"] archive source code
#	-@for fmt in `echo $(FMTS) | tr ',' ' '` ; do \
#		case $$fmt in \
#			7z) echo "### target/$(distdir).7z ###" ; \
#				rm -f target/$(distdir).7z ; \
#				(cd target ; 7za a -t7z -mx=9 $(distdir).7z $(distdir)) ;; \
#			zip) echo "### target/$(distdir).zip ###" ; \
#				rm -f target/$(distdir).zip ; \
#				(cd target ; zip -9 -q -r $(distdir).zip $(distdir)) ;; \
#			*) tarext=`echo $$fmt | grep -e '^tar$$' -e '^tar.xz$$' -e '^tar.zst$$' -e '^tar.bz2$$' || echo tar.gz` ; \
#				echo "### target/$(distdir).$$tarext ###" ; \
#				rm -f target/$(distdir).$$tarext ; \
#				(cd target ; tar --posix -L -caf $(distdir).$$tarext $(distdir)) ;; \
#		esac \
#	done
#	-@rm -r target/$(distdir)
jar: ## jar archive
	-jar -uf $(dist_jar) src Makefile *.xml Makefile-*.mk *.sh
	-jar -uf $(dist_jar) -C target docs
javadoc: ## (javadoc) generate documentation
	-rm -fr target/docs/javadoc ; mkdir -p target/docs/javadoc
	-if [ 0 -lt `find . -name '*.java' 2> /dev/null | wc -l` ] ; then \
		javadoc $(JAVADOC_OPTS) -classpath $(classpath_test):$(classes_dir):$(classes_test_dir) -d target/docs/javadoc `find src -name '*.java' -a ! -name 'package*.*'` ; \
	fi
scaladoc: ## (scaladoc) generate documentation
	-rm -fr target/docs/scaladoc ; mkdir -p target/docs/scaladoc
	-if [ 0 -lt `find . -name '*.scala' 2> /dev/null | wc -l` ] ; then \
		scaladoc $(SCALADOC_OPTS) -classpath $(classpath_test):$(classes_dir):$(classes_test_dir) -d target/docs/scaladoc `find src -name '*.scala' -a ! -name 'package*.*'` ; \
	fi
#groovydoc: ## (groovydoc) generate documentation
#	-rm -fr target/docs/groovydoc ; mkdir -p target/docs/groovydoc
#	-if [ 0 -lt `find . -name '*.groovy' 2> /dev/null | wc -l` ] ; then \
#		groovydoc $(GROOVYDOC_OPTS) -classpath $(classpath_test):$(classes_dir):$(classes_test_dir) -d target/docs/groovydoc `find src -name '*.groovy' -a ! -name 'package*.*'` ; \
#	fi
checkstyle: ## (checkstyle) lint check
#	-checkstyle -c config/sun_checks.xml src/main/java
	-java -jar $(HOME)/.ant/lib/ivy.jar -mode dynamic -types jar \
		-confs default -dependency com.puppycrawl.tools checkstyle '[9.2.1,)' \
		-main com.puppycrawl.tools.checkstyle.Main -- \
		-c config/sun_checks.xml src/main/java
scalastyle: ## (scalastyle) lint check
#	#-scalastyle -c config/scalastyle_confi.xml src/main/scala
	-java -jar $(HOME)/.ant/lib/ivy.jar -mode dynamic -types jar \
		-confs default -dependency com.beautiful-scala scalastyle_$(SCALA_COMPAT) \
		'[1.4.0,)' -main org.scalastyle.Main -- \
		-c config/scalastyle_config.xml src/main/scala
#codenarc: ## (codenarc) lint check
#	-java -jar $(HOME)/.ant/lib/ivy.jar -mode dynamic -types jar \
#		-confs default -dependency org.codenarc CodeNarc \
#		'[2.2.0,)' -main org.codenarc.CodeNarc -- \
#		-basedir=src/main/groovy -report=html:target/CodeNarcReport.html
cover: ## generate code coverage
#	-java -Djava.library.path=$(java_lib_path) $(JAVA_ARGS) -javaagent:$(HOME)/.ant/lib/jacocoagent-runtime.jar=destfile=target/jacoco.exec,append=true,output=file -jar $(dist_test_jar) \
#		$(TOPTS)
	-ant -f build-jacoco.xml -Djava.library.path=$(java_lib_path) \
		-Ddist_test.jar=$(dist_test_jar) cover
report: ## report code coverage
	-ant -f build-jacoco.xml -Ddist.jar=$(dist_jar) report
#cover_jcov: $(dist_test_jar) ## (jcov) generate code coverage
#	-java $(JAVA_ARGS) -jar $(JAVA_LIB)/jcov.jar Instr -verbose \
#		-t target/template.xml -o target/instrumented $(dist_jar)
#	-cp $(dist_test_jar) target/instrumented/
#	-java -Djava.library.path=$(java_lib_path) $(JAVA_ARGS) -Xbootclasspath/a:$(JAVA_LIB)/jcov_file_saver.jar \
#		-jar target/instrumented/`basename $(dist_test_jar)` \
#		-Djcov.template=target/template.xml \
#		-Djcov.file=target/result.xml $(TOPTS)
#report_jcov: ## (jcov) report code coverage
#	-java $(JAVA_ARGS) -jar $(JAVA_LIB)/jcov.jar Merger -verbose \
#		-o target/merged.xml target/template.xml target/result.xml
#	-java $(JAVA_ARGS) -jar $(JAVA_LIB)/jcov.jar RepGen -verbose -src src/main \
#		-include $(groupid).$(parent).Util -fmt html -o target/cov target/merged.xml

DEBUGGER = ddd --jdb        # ddd --jdb; jdb

run: $(dist_jar) ## run main [ARGS=""]
#	export [DY]LD_LIBRARY_PATH=. # ([da|ba|z]sh Linux)
#	setenv [DY]LD_LIBRARY_PATH . # (tcsh FreeBSD)
	-if [ ! "" = "$(main_class)" ] ; then \
		java -Djava.library.path=$(java_lib_path) $(JAVA_ARGS) \
			-jar $(dist_jar) $(ARGS) ; \
	fi
debug: $(dist_jar) ## debug main [ARGS=""]
	-if [ ! "" = "$(main_class)" ] ; then \
		$(DEBUGGER) -Djava.library.path=$(java_lib_path) \
			-classpath $(dist_jar) $(main_class) $(ARGS) ; \
	fi
