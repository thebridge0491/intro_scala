# Dependency manager tasks (Ivy, Maven) Makefile script.

depnmgr ?= ivy  # ivy or maven
version ?= 0
SCALA_COMPAT ?= 2.13
rsrc_path ?= src/main/resources
IVY ?= java -Dscala.compat=$(SCALA_COMPAT) -Divy.cache.ttl.default=eternal -jar $(HOME)/.ant/lib/ivy.jar -properties $(rsrc_path)/versions.properties
MVN ?= ./mvnw -Dscala.compat=$(SCALA_COMPAT)

.PHONY: resolve retrieve publish
resolve: ## resolve depns
	mkdir -p target
	-@if [ "maven" = "$(depnmgr)" ] ; then \
		$(MVN) dependency:resolve ; \
		$(MVN) dependency:build-classpath -DincludeScope=compile \
			-Dmdep.outputFile=target/classpath_compile.txt ; \
		$(MVN) dependency:build-classpath -DincludeScope=runtime \
			-Dmdep.outputFile=target/classpath_runtime.txt ; \
		$(MVN) dependency:build-classpath -DincludeScope=test \
			-Dmdep.outputFile=target/classpath_test.txt ; \
	else \
		$(IVY) -confs compile -cachepath target/classpath_compile.txt ; \
		$(IVY) -confs runtime -cachepath target/classpath_runtime.txt ; \
		$(IVY) -confs test -cachepath target/classpath_test.txt ; \
	fi

retrieve: resolve ## retrieve depns
	-@if [ "maven" = "$(depnmgr)" ] ; then \
		$(MVN) dependency:copy-dependencies -DoutputDirectory=target/lib ; \
	else \
		$(IVY) -symlink -retrieve "target/lib/[artifact]-[revision](-[classifier]).[ext]" ; \
	fi

publish: ## publish project artifact(s)
	-@if [ "maven" = "$(depnmgr)" ] ; then \
		$(MVN) install ; \
	else \
		$(IVY) -revision $(version) -deliverto "target/ivy-[revision].xml" \
			-publishpattern "target/[artifact]-[revision](-[classifier]).[ext]" \
			-publish local -overwrite ; \
	fi
