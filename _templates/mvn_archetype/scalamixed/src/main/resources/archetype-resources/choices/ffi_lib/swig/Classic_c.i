#set( $symbol_pound = '#' )
#set( $symbol_dollar = '$' )
#set( $symbol_escape = '\' )
%module Classic_c

%{
${symbol_pound}include <stdlib.h>
${symbol_pound}include "intro_c/classic.h"
%}

%include "intro_c/classic.h"
//extern unsigned long fact_i(const unsigned long n);
//extern unsigned long fact_lp(const unsigned long n);

//extern float expt_i(const float b, const float n);
//extern float expt_lp(const float b, const float n);

// swig -java -v -package ${package} -I${symbol_dollar}(PREFIX)/include -outdir src/main/java/${packageInPathFormat}/ -o build/classic_c_wrap.c src/main/scala/${packageInPathFormat}/Classic_c.i
