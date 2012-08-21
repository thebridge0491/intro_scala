#set( $symbol_pound = '#' )
#set( $symbol_dollar = '$' )
#set( $symbol_escape = '\' )
${symbol_pound} Targets rakefile script.
require 'rake/clean'
require 'rake/packagetask'

[CLEAN, CLOBBER, Rake::FileList::DEFAULT_IGNORE_PATTERNS].each{|a| a.clear}
CLEAN.include('**/*.o', '*.log', '**/.coverage')
CLOBBER.include('target/*', 'target/.??*')

JAVADOC_OPTS = "${symbol_pound}{ENV['JAVADOC_OPTS']} -use -private -version -author "
SCALADOC_OPTS = "${symbol_pound}{ENV['SCALADOC_OPTS']} -author "

desc 'Help info'
task :help do
  puts "===== subproject: ${symbol_pound}{VARS.proj} =====\nHelp: ${symbol_pound}{RAKE} [DEBUG=1] [task]"
  sh "${symbol_pound}{RAKE} -T"
end

desc 'Run tests: rake test\[topt1,topt2\]'
task :test, [:topt1] => :testCompile do |t, topts|
  sh "java -Djava.library.path=${symbol_pound}{JAVA_LIB_PATH} ${symbol_pound}{VARS.java_args} -jar ${symbol_pound}{VARS.dist_test_jar} ${symbol_pound}{topts[:topt1]} ${symbol_pound}{topts.extras.join(' ')} || true"
end

${symbol_pound}file "target/${symbol_pound}{VARS.parent}-${symbol_pound}{VARS.version}" do |p|
${symbol_pound}  mkdir_p(p.name)
${symbol_pound}  ${symbol_pound} sh "zip -9 -q --exclude @exclude.lst -r - . | unzip -od ${symbol_pound}{p.name} -"
${symbol_pound}  sh "tar --format=posix --dereference --exclude-from=exclude.lst -cf - . | tar -xpf - -C ${symbol_pound}{p.name}"
${symbol_pound}end
${symbol_pound}
${symbol_pound}if defined? Rake::PackageTask
${symbol_pound}  Rake::PackageTask.new(VARS.parent, VARS.version) do |p|
${symbol_pound}    ${symbol_pound} task("target/${symbol_pound}{parent}-${symbol_pound}{version}").invoke
${symbol_pound}    
${symbol_pound}    ENV.fetch('FMTS', 'tar.gz').split(',').each{|fmt|
${symbol_pound}      if p.respond_to? "need_${symbol_pound}{fmt.tr('.', '_')}="
${symbol_pound}        p.send("need_${symbol_pound}{fmt.tr('.', '_')}=", true)
${symbol_pound}      else
${symbol_pound}        p.need_tar_gz = true
${symbol_pound}      end
${symbol_pound}    }
${symbol_pound}    task(:package).add_description "[FMTS=${symbol_pound}{ENV.fetch('FMTS', 'tar.gz')}]"
${symbol_pound}    task(:repackage).add_description "[FMTS=${symbol_pound}{ENV.fetch('FMTS', 'tar.gz')}]"
${symbol_pound}  end
${symbol_pound}else
${symbol_pound}  desc "[FMTS=${symbol_pound}{ENV.fetch('FMTS', 'tar.gz')}] Package project distribution"
${symbol_pound}  task :dist => ["target/${symbol_pound}{parent}-${symbol_pound}{version}"] do |t|
${symbol_pound}    distdir = "${symbol_pound}{parent}-${symbol_pound}{version}"
${symbol_pound}    
${symbol_pound}    ENV.fetch('FMTS', 'tar.gz').split(',').each{|fmt|
${symbol_pound}      case fmt
${symbol_pound}      when 'zip'
${symbol_pound}        rm_rf("target/${symbol_pound}{distdir}.zip") || true
${symbol_pound}        cd('target') {sh "zip -9 -q -r ${symbol_pound}{distdir}.zip ${symbol_pound}{distdir}" || true}
${symbol_pound}      else
${symbol_pound}        ${symbol_pound}tarext = `echo ${symbol_pound}{fmt} | grep -e '^tar$' -e '^tar.xz$' -e '^tar.bz2$' || echo tar.gz`.chomp
${symbol_pound}        tarext = fmt.match(%r{(^tar$|^tar.xz$|^tar.bz2$)}) ? fmt : 'tar.gz'
${symbol_pound}        rm_rf("target/${symbol_pound}{distdir}.${symbol_pound}{tarext}") || true
${symbol_pound}        cd('target') {sh "tar --posix -L -caf ${symbol_pound}{distdir}.${symbol_pound}{tarext} ${symbol_pound}{distdir}" || true}
${symbol_pound}      end
${symbol_pound}    }
${symbol_pound}    rm_rf("target/${symbol_pound}{distdir}") || true
${symbol_pound}  end
${symbol_pound}end

desc 'Jar archive project'
task :jar do
  sh "jar -uf target/${symbol_pound}{VARS.dist_jar} src rakefile *.xml rakefile-*.rb *.sh"
  sh "jar -uf target/${symbol_pound}{VARS.dist_jar} -C target docs" \
    if File.exists?("target/docs")
end

desc 'Generate documentation (javadoc)'
task :javadoc do
  rm_rf("target/docs/javadoc")
  mkdir_p("target/docs/javadoc")
  sh "javadoc ${symbol_pound}{JAVADOC_OPTS} -classpath ${symbol_pound}{VARS.classpath_test}:${symbol_pound}{CLASSES_DIR}:${symbol_pound}{CLASSES_TEST_DIR} -d target/docs/javadoc ${symbol_pound}{FileList['src/main/java/**/*.java']}" if 0 != FileList['src/main/java/**/*.java'].size
end

desc 'Generate documentation (scaladoc)'
task :scaladoc do
  rm_rf("target/docs/scaladoc")
  mkdir_p("target/docs/scaladoc")
  sh "scaladoc ${symbol_pound}{SCALADOC_OPTS} -classpath ${symbol_pound}{VARS.classpath_test}:${symbol_pound}{CLASSES_DIR}:${symbol_pound}{CLASSES_TEST_DIR} -d target/docs/scaladoc ${symbol_pound}{FileList['src/main/scala/**/*.scala']}" if 0 != FileList['src/main/scala/**/*.scala'].size
end

desc 'Lint check (checkstyle)'
task :checkstyle do
  ${symbol_pound}sh "checkstyle -c config/sun_checks.xml src/main/java || true"
  sh "java -jar ${symbol_pound}{ENV['HOME']}/.ant/lib/ivy.jar -mode dynamic -types jar -confs default -dependency com.puppycrawl.tools checkstyle '[5.5,)' -main com.puppycrawl.tools.checkstyle.Main -- -c config/sun_checks.xml src/main/java || true"
end

desc 'Lint check (scalastyle)'
task :scalastyle do
  ${symbol_pound}sh "scalastyle -c config/scalastyle_config.xml src/main/scala || true"
  sh "java -jar ${symbol_pound}{ENV['HOME']}/.ant/lib/ivy.jar -mode dynamic -types jar \
		-confs default -dependency org.scalastyle scalastyle_${symbol_pound}{SCALA_COMPAT} \
		'[0.1.0,)' -main org.scalastyle.Main -- \
		-c config/scalastyle_config.xml src/main/scala || true"
end

desc 'Generate code coverage (jacoco): rake cover\[topt1,topt2\]'
task :cover, [:topt1] => :testCompile do |t, topts|
  ${symbol_pound}sh "java -Djava.library.path=${symbol_pound}{JAVA_LIB_PATH} ${symbol_pound}{VARS.java_args} -javaagent:${symbol_pound}{ENV['HOME']}/.ant/lib/jacocoagent-runtime.jar=destfile=target/jacoco.exec,append=true,output=file -jar ${symbol_pound}{VARS.dist_test_jar} ${symbol_pound}{topts[:topt1]} ${symbol_pound}{topts.extras.join(' ')} || true"
  sh "ant -f build-jacoco.xml -Djava.library.path=${symbol_pound}{JAVA_LIB_PATH} -Ddist_test.jar=${symbol_pound}{VARS.dist_test_jar} cover"
end

desc 'Report code coverage (jacoco): rake report'
task :report do
  sh "ant -f build-jacoco.xml -Ddist.jar=${symbol_pound}{VARS.dist_jar} report"
end

${symbol_pound}desc 'Generate code coverage (jcov): rake cover_jcov\[topt1,topt2\]'
${symbol_pound}task :cover_jcov, [:topt1] => :testCompile do |t, topts|
${symbol_pound}  sh "java ${symbol_pound}{VARS.java_args} -jar ${symbol_pound}{JAVA_LIB}/jcov.jar Instr -verbose -t target/template.xml -o target/instrumented ${symbol_pound}{VARS.dist_jar}"
${symbol_pound}  sh "cp ${symbol_pound}{VARS.dist_test_jar} target/instrumented/ || true"
${symbol_pound}  sh "java -Djava.library.path=${symbol_pound}{JAVA_LIB_PATH} ${symbol_pound}{VARS.java_args} -Djcov.template=target/template.xml -Djcov.file=target/result.xml -Xbootclasspath/a:${symbol_pound}{JAVA_LIB}/jcov_file_saver.jar -jar target/instrumented/${symbol_pound}{VARS.parent}-${symbol_pound}{VARS.pkg}-${symbol_pound}{VARS.version}-tests.jar || true"
${symbol_pound}end
${symbol_pound}
${symbol_pound}desc 'Report code coverage (jcov): rake report_jcov'
${symbol_pound}task :report_jcov do
${symbol_pound}  sh "java ${symbol_pound}{VARS.java_args} -jar ${symbol_pound}{JAVA_LIB}/jcov.jar Merger -verbose -o target/merged.xml target/template.xml target/result.xml"
${symbol_pound}  sh "java ${symbol_pound}{VARS.java_args} -jar ${symbol_pound}{JAVA_LIB}/jcov.jar RepGen -verbose -src src/main -include ${symbol_pound}{VARS.groupid}.${symbol_pound}{VARS.parent}.Util -fmt html -o target/cov target/merged.xml"
${symbol_pound}end

debugger = 'ddd --jdb ' ${symbol_pound} ddd --jdb; jdb

desc 'Run program: rake run\[arg1,arg2\]'
task :run, [:arg1] => VARS.dist_jar do |t, args|
  ${symbol_pound}export LD_LIBRARY_PATH=${symbol_pound}{ENV['PWD']}/lib:${symbol_pound}{ENV['LD_LIBRARY_PATH']} ${symbol_pound} dash
  ${symbol_pound}setenv LD_LIBRARY_PATH ${symbol_pound}{ENV['PWD']}/lib:${symbol_pound}{ENV['LD_LIBRARY_PATH']} ${symbol_pound} tcsh
  if '' != VARS.main_class
    sh "java -Djava.library.path=${symbol_pound}{JAVA_LIB_PATH} ${symbol_pound}{VARS.java_args} -jar ${symbol_pound}{t.source} ${symbol_pound}{args[:arg1]} ${symbol_pound}{args.extras.join(' ')}"
  end
end
desc 'Debug program: rake debug\[arg1,arg2\]'
task :debug, [:arg1] => VARS.dist_jar do |t, args|
  if '' != VARS.main_class
    sh "${symbol_pound}{debugger} -Djava.library.path=${symbol_pound}{JAVA_LIB_PATH} -classpath ${symbol_pound}{t.source} ${symbol_pound}{VARS.main_class} ${symbol_pound}{args[:arg1]} ${symbol_pound}{args.extras.join(' ')}"
  end
end
