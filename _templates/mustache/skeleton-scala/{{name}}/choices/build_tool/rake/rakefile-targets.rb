# Targets rakefile script.
require 'rake/clean'
require 'rake/packagetask'

[CLEAN, CLOBBER, Rake::FileList::DEFAULT_IGNORE_PATTERNS].each{|a| a.clear}
CLEAN.include('**/*.o', '*.log', '**/.coverage', 'core*')
CLOBBER.include('target/*', 'target/.??*')

JAVADOC_OPTS = "#{ENV['JAVADOC_OPTS']} -use -private -version -author "
SCALADOC_OPTS = "#{ENV['SCALADOC_OPTS']} -author "

desc 'Help info'
task :help do
  puts "===== subproject: #{VARS.proj} =====\nHelp: #{RAKE} [DEBUG=1] [task]"
  sh "#{RAKE} -T"
end

desc 'Run tests: rake test\[topt1,topt2\]'
task :test, [:topt1] => :testCompile do |t, topts|
  sh "java -Djava.library.path=#{JAVA_LIB_PATH} #{VARS.java_args} -jar #{VARS.dist_test_jar} #{topts[:topt1]} #{topts.extras.join(' ')} || true"
end

#file "target/#{VARS.name}-#{VARS.version}" do |p|
#  mkdir_p(p.name)
#  # sh "zip -9 -q -x @exclude.lst -r - . | unzip -od #{p.name} -"
#  sh "tar --posix -L -X exclude.lst -cf - . | tar -xpf - -C #{p.name}"
#end
#if defined? Rake::PackageTask
#  Rake::PackageTask.new(VARS.name, VARS.version) do |p|
#    # task("target/#{VARS.name}-#{VARS.version}").invoke
#    
#    ENV.fetch('FMTS', 'tar.gz').split(',').each{|fmt|
#      if p.respond_to? "need_#{fmt.tr('.', '_')}="
#        p.send("need_#{fmt.tr('.', '_')}=", true)
#      else
#        p.need_tar_gz = true
#      end
#    }
#    task(:package).add_description "[FMTS=#{ENV.fetch('FMTS', 'tar.gz')}]"
#    task(:repackage).add_description "[FMTS=#{ENV.fetch('FMTS', 'tar.gz')}]"
#  end
#else
#  desc "[FMTS=#{ENV.fetch('FMTS', 'tar.gz')}] Package project distribution"
#  task :dist => ["target/#{VARS.name}-#{VARS.version}"] do |t|
#    distdir = "#{VARS.name}-#{VARS.version}"
#    
#    ENV.fetch('FMTS', 'tar.gz').split(',').each{|fmt|
#      case fmt
#      when 'zip'
#        rm_rf("target/#{distdir}.zip") || true
#        cd('target') {sh "zip -9 -q -r #{distdir}.zip #{distdir}" || true}
#      else
#        # tarext = `echo #{fmt} | grep -e '^tar$' -e '^tar.xz$' -e '^tar.bz2$' || echo tar.gz`.chomp
#        tarext = fmt.match(%r{(^tar$|^tar.xz$|^tar.bz2$)}) ? fmt : 'tar.gz'
#        rm_rf("target/#{distdir}.#{tarext}") || true
#        cd('target') {sh "tar --posix -L -caf #{distdir}.#{tarext} #{distdir}" || true}
#      end
#    }
#  end
#end

desc 'Jar archive project'
task :jar do
  sh "jar -uf target/#{VARS.dist_jar} src rakefile *.xml rakefile-*.rb *.sh"
  sh "jar -uf target/#{VARS.dist_jar} -C target docs" \
    if File.exists?("target/docs")
end

desc 'Generate documentation (javadoc)'
task :javadoc do
  rm_rf("target/docs/javadoc")
  mkdir_p("target/docs/javadoc")
  sh "javadoc #{JAVADOC_OPTS} -classpath #{VARS.classpath_test}:#{CLASSES_DIR}:#{CLASSES_TEST_DIR} -d target/docs/javadoc #{FileList['src/main/java/**/*.java']}" if 0 != FileList['src/main/java/**/*.java'].size
end

desc 'Generate documentation (scaladoc)'
task :scaladoc do
  rm_rf("target/docs/scaladoc")
  mkdir_p("target/docs/scaladoc")
  sh "scaladoc #{SCALADOC_OPTS} -classpath #{VARS.classpath_test}:#{CLASSES_DIR}:#{CLASSES_TEST_DIR} -d target/docs/scaladoc #{FileList['src/main/scala/**/*.scala']}" if 0 != FileList['src/main/scala/**/*.scala'].size
end

desc 'Lint check (checkstyle)'
task :checkstyle do
  #sh "checkstyle -c config/sun_checks.xml src/main/java || true"
  sh "java -jar #{ENV['HOME']}/.ant/lib/ivy.jar -mode dynamic -types jar -confs default -dependency com.puppycrawl.tools checkstyle '[5.5,)' -main com.puppycrawl.tools.checkstyle.Main -- -c config/sun_checks.xml src/main/java || true"
end

desc 'Lint check (scalastyle)'
task :scalastyle do
  #sh "scalastyle -c config/scalastyle_config.xml src/main/scala || true"
  sh "java -jar #{ENV['HOME']}/.ant/lib/ivy.jar -mode dynamic -types jar \
		-confs default -dependency org.scalastyle scalastyle_#{SCALA_COMPAT} \
		'[0.1.0,)' -main org.scalastyle.Main -- \
		-c config/scalastyle_config.xml src/main/scala || true"
end

desc 'Generate code coverage (jacoco): rake cover\[topt1,topt2\]'
task :cover, [:topt1] => :testCompile do |t, topts|
  #sh "java -Djava.library.path=#{JAVA_LIB_PATH} #{VARS.java_args} -javaagent:#{ENV['HOME']}/.ant/lib/jacocoagent-runtime.jar=destfile=target/jacoco.exec,append=true,output=file -jar #{VARS.dist_test_jar} #{topts[:topt1]} #{topts.extras.join(' ')} || true"
  sh "ant -f build-jacoco.xml -Djava.library.path=#{JAVA_LIB_PATH} -Ddist_test.jar=#{VARS.dist_test_jar} cover"
end

desc 'Report code coverage (jacoco): rake report'
task :report do
  sh "ant -f build-jacoco.xml -Ddist.jar=#{VARS.dist_jar} report"
end

#desc 'Generate code coverage (jcov): rake cover_jcov\[topt1,topt2\]'
#task :cover_jcov, [:topt1] => :testCompile do |t, topts|
#  sh "java #{VARS.java_args} -jar #{JAVA_LIB}/jcov.jar Instr -verbose -t target/template.xml -o target/instrumented #{VARS.dist_jar}"
#  sh "cp #{VARS.dist_test_jar} target/instrumented/ || true"
#  sh "java -Djava.library.path=#{JAVA_LIB_PATH} #{VARS.java_args} -Djcov.template=target/template.xml -Djcov.file=target/result.xml -Xbootclasspath/a:#{JAVA_LIB}/jcov_file_saver.jar -jar target/instrumented/#{VARS.parent}-#{VARS.pkg}-#{VARS.version}-tests.jar || true"
#end
#
#desc 'Report code coverage (jcov): rake report_jcov'
#task :report_jcov do
#  sh "java #{VARS.java_args} -jar #{JAVA_LIB}/jcov.jar Merger -verbose -o target/merged.xml target/template.xml target/result.xml"
#  sh "java #{VARS.java_args} -jar #{JAVA_LIB}/jcov.jar RepGen -verbose -src src/main -include #{VARS.groupid}.#{VARS.parent}.Util -fmt html -o target/cov target/merged.xml"
#end

debugger = 'ddd --jdb ' # ddd --jdb; jdb

desc 'Run program: rake run\[arg1,arg2\]'
task :run, [:arg1] => VARS.dist_jar do |t, args|
  #export LD_LIBRARY_PATH=#{ENV['PWD']}/lib:#{ENV['LD_LIBRARY_PATH']} # dash
  #setenv LD_LIBRARY_PATH #{ENV['PWD']}/lib:#{ENV['LD_LIBRARY_PATH']} # tcsh
  if '' != VARS.main_class
    sh "java -Djava.library.path=#{JAVA_LIB_PATH} #{VARS.java_args} -jar #{t.source} #{args[:arg1]} #{args.extras.join(' ')}"
  end
end
desc 'Debug program: rake debug\[arg1,arg2\]'
task :debug, [:arg1] => VARS.dist_jar do |t, args|
  if '' != VARS.main_class
    sh "#{debugger} -Djava.library.path=#{JAVA_LIB_PATH} -classpath #{t.source} #{VARS.main_class} #{args[:arg1]} #{args.extras.join(' ')}"
  end
end
