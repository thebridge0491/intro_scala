PREFIX = ENV['prefix'] ? ENV['prefix'] : '/usr/local'

# FFI auxiliary rakefile script

PKG_CONFIG = "pkg-config --with-path=#{PREFIX}/lib/pkgconfig"

CC = 'clang'		# clang | gcc

if 'Darwin' == `sh -c 'uname -s 2>/dev/null || echo not'`.chomp
  SHLIBEXT = 'dylib'
else
  SHLIBEXT = 'so'
  ENV['LDFLAGS'] = "#{ENV['LDFLAGS']} -Wl,--enable-new-dtags"
end

FFI_LIBDIR = `#{PKG_CONFIG} --variable=libdir intro_c-practice || echo .`
FFI_INCDIR = `#{PKG_CONFIG} --variable=includedir intro_c-practice || echo .`
ENV['LD_LIBRARY_PATH'] = "#{ENV['LD_LIBRARY_PATH']}:#{FFI_LIBDIR}"
sh "export LD_LIBRARY_PATH"

JAVA_INCDIR = `sh -c "find #{ENV['JAVA_HOME'] -type f -name 'jni.h' -exec dirname {} \;"`.chomp

VARS.cppflags = "#{ENV['CPPFLAGS']} -I#{JAVA_INCDIR} -I#{JAVA_INCDIR}/linux -I#{JAVA_INCDIR}/freebsd -I#{FFI_INCDIR}"
VARS.ldflags = "#{ENV['LDFLAGS']} -Wl,-rpath,'$ORIGIN/:#{FFI_LIBDIR}' -L#{FFI_LIBDIR}"
VARS.cflags = "#{ENV['CFLAGS']} -Wall -pedantic -std=c99 -m64"
VARS.arflags = 'rvcs'
VARS.ldlibs = "#{ENV['LDLIBS']} -lintro_c-practice"

src_c = FileList["build/classic_c_wrap.c"]

file "build/lib#{VARS.proj}_stubs.a" => src_c.ext('.o').map { |o| 
  o.sub(/\.\.\/+/, '') }
file "build/lib#{VARS.proj}_stubs.#{SHLIBEXT}" => src_c

rule '.a' do |t| 
  sh "ar #{VARS.arflags} #{t.name} #{t.prerequisites.join(' ')}"
end
rule '.so' do |t| 
  sh "#{LINK_c.call(VARS, t)} -fPIC -shared -o #{t.name} #{VARS.ldlibs}" || true
end
rule '.dylib' do |t| 
  sh "#{LINK_c.call(VARS, t)} -fPIC -dynamiclib -undefined suppress -flat_namespace -o #{t.name} #{VARS.ldlibs}" || true
end

LINK_c = lambda { |v, t| 
  "#{CC} #{v.cppflags} #{v.ldflags} #{v.cflags} #{t.prerequisites.join(' ')}" }

rule(/\.o$/ => [lambda { |src|              # rule '.o' => '.c'
    src.sub(/\.[^.]+$/, '.c') } ]) do |t|
  sh "#{CC} #{VARS.cppflags} #{VARS.cflags} -c -o #{t.name} #{t.source}"
end

desc 'Compile FFI auxiliary products'
task :auxffi => ["build/lib#{VARS.proj}_stubs.a", 
  "build/lib#{VARS.proj}_stubs.#{SHLIBEXT}"]

file "build/classic_c_wrap.c" => ["src/main/java/#{namespace_path}/Classic_c.i"] do |t|
  mkdir_p('build/classes')
  sh "swig -v -java -package #{VARS.groupid}.#{VARS.parent}.#{pkg} -I#{FFI_INCDIR} -outdir src/main/java/#{namespace_path}/ -o #{t.name} #{t.prerequisites.join(' ')} || true"
end

desc 'Prepare Swig files'
task :prep_swig => ["build/classic_c_wrap.c"]
desc 'Clean Swig files'
task :clean_swig do
  rm_rf(FileList["src/main/java/#{namespace_path}/Classic_c*.java"]) || true
end
