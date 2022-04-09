#set( $symbol_pound = '#' )
#set( $symbol_dollar = '$' )
#set( $symbol_escape = '\' )
.POSIX:
.SUFFIXES:
PREFIX ?= /usr/local
VPATH ?= .

${symbol_pound} FFI auxiliary makefile script
.SUFFIXES: .s .o .a .h .c

PKG_CONFIG ?= pkg-config --with-path=${symbol_dollar}(PREFIX)/lib/pkgconfig

groupid ?= ${groupId}
pkg ?= ${name}
parent ?= ${parent}
namespace_path ?= ${symbol_dollar}(shell echo ${symbol_dollar}(groupid).${symbol_dollar}(parent).${symbol_dollar}(pkg) | sed 'y|.|/|')
proj ?= ${symbol_dollar}(groupid).${symbol_dollar}(parent).${symbol_dollar}(pkg)

CC ?= clang		${symbol_pound} clang | gcc

ifeq (${symbol_dollar}(shell sh -c 'uname -s 2>/dev/null || echo not'),Darwin)
sosuffix = dylib
else
sosuffix = so
LDFLAGS := ${symbol_dollar}(LDFLAGS) -Wl,--enable-new-dtags
endif

ffi_libdir = ${symbol_dollar}(shell ${symbol_dollar}(PKG_CONFIG) --variable=libdir intro_c-practice || echo .)
ffi_incdir = ${symbol_dollar}(shell ${symbol_dollar}(PKG_CONFIG) --variable=includedir intro_c-practice || echo .)
LD_LIBRARY_PATH := ${symbol_dollar}(LD_LIBRARY_PATH):${symbol_dollar}(ffi_libdir)
export LD_LIBRARY_PATH

java_incdir = ${symbol_dollar}(shell find ${symbol_dollar}(JAVA_HOME) -type f -name 'jni.h' -exec dirname {} ${symbol_escape};)

CPPFLAGS := ${symbol_dollar}(CPPFLAGS) -I${symbol_dollar}(java_incdir) -I${symbol_dollar}(java_incdir)/linux -I${symbol_dollar}(java_incdir)/freebsd -I${symbol_dollar}(ffi_incdir)
LDFLAGS := ${symbol_dollar}(LDFLAGS) -Wl,-rpath,'${symbol_dollar}${symbol_dollar}ORIGIN/:${symbol_dollar}(ffi_libdir)' -L${symbol_dollar}(ffi_libdir)
CFLAGS := ${symbol_dollar}(CFLAGS) -Wall -pedantic -std=c99
ARFLAGS = rvcs
LDLIBS := ${symbol_dollar}(LDLIBS) -lintro_c-practice

src_c = build/classic_c_wrap.c

build/lib${symbol_dollar}(proj)_stubs.${symbol_dollar}(sosuffix) : ${symbol_dollar}(src_c)

build/%.so :
	-${symbol_dollar}(LINK.c) -fPIC -shared ${symbol_dollar}^ -o ${symbol_dollar}@ ${symbol_dollar}(LDLIBS)
build/%.dylib :
	-${symbol_dollar}(LINK.c) -fPIC -dynamiclib -undefined suppress -flat_namespace ${symbol_dollar}^ -o ${symbol_dollar}@ ${symbol_dollar}(LDLIBS)

.PHONY: auxffi
auxffi: build/lib${symbol_dollar}(proj)_stubs.a(${symbol_dollar}(src_c:.c=.o)) build/lib${symbol_dollar}(proj)_stubs.${symbol_dollar}(sosuffix) ${symbol_pound}${symbol_pound} compile FFI auxiliary products

.PHONY: prep_swig clean_swig
prep_swig build/classic_c_wrap.c : src/main/scala/${symbol_dollar}(namespace_path)/Classic_c.i ${symbol_pound}${symbol_pound} prepare Swig files
	-mkdir -p build/classes
	-swig -v -java -package ${symbol_dollar}(groupid).${symbol_dollar}(parent).${symbol_dollar}(pkg) -I${symbol_dollar}(ffi_incdir) -outdir src/main/java/${symbol_dollar}(namespace_path)/ -o build/classic_c_wrap.c src/main/scala/${symbol_dollar}(namespace_path)/Classic_c.i
clean_swig : ${symbol_pound}${symbol_pound} clean Swig files
	-rm -fr src/main/java/${symbol_dollar}(namespace_path)/Classic_c*.java
