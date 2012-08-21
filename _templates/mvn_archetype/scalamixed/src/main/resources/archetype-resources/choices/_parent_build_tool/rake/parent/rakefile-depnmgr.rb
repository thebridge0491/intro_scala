# Dependency manager tasks (Ivy, Maven) rakefile script.

DEPNMGR = ENV['depnmgr'] ? ENV['depnmgr'] : 'ivy'
VERSION = ENV['version'] ? ENV['version'] : '0'
SCALA_COMPAT = ENV['SCALA_COMPAT'] ? ENV['SCALA_COMPAT'] : '2.9'

IVY = "java -Dscala.compat=#{SCALA_COMPAT} -Divy.cache.ttl.default=eternal -jar #{ENV['HOME']}/.ant/lib/ivy.jar"
MVN = "mvn -Dscala.compat=#{SCALA_COMPAT}"

desc "Resolve depns [depnmgr=#{DEPNMGR}]"
task :resolve do
  mkdir_p("target")
  if 'maven' == DEPNMGR
    sh "#{MVN} dependency:resolve"
    sh "#{MVN} dependency:build-classpath -DincludeScope=compile -Dmdep.outputFile=target/classpath_compile.txt"
    sh "#{MVN} dependency:build-classpath -DincludeScope=runtime -Dmdep.outputFile=target/classpath_runtime.txt"
    sh "#{MVN} dependency:build-classpath -DincludeScope=test -Dmdep.outputFile=target/classpath_test.txt"
  else
    sh "#{IVY} -confs compile -cachepath target/classpath_compile.txt"
    sh "#{IVY} -confs runtime -cachepath target/classpath_runtime.txt"
    sh "#{IVY} -confs test -cachepath target/classpath_test.txt"
  end
end

desc "Retrieve depns [depnmgr=#{DEPNMGR}]"
task :retrieve => :resolve do
  if 'maven' == DEPNMGR
    sh "#{MVN} dependency:copy-dependencies -DoutputDirectory=target/lib"
  else
    sh "#{IVY} -symlink -retrieve 'target/lib/[artifact]-[revision](-[classifier]).[ext]'"
  end
end

desc "Publish project artifact [depnmgr=#{DEPNMGR} version=#{VERSION}]"
task :publish do
  if 'maven' == DEPNMGR
    sh "#{MVN} install"
  else
    sh "#{IVY} -revision #{VERSION} -deliverto 'target/ivy-[revision].xml' -publishpattern 'target/[artifact]-[revision](-[classifier]).[ext]' -publish local -overwrite"
  end
end
