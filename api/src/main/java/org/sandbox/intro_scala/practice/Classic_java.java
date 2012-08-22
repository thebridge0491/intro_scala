package org.sandbox.intro_scala.practice;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/** DocComment:
 * <p>Brief description.</p> */
public class Classic_java {
	private static final Logger pracLogger = LoggerFactory.getLogger("prac");
	
    public static long fact_lp(final long n) {
        long acc = 1L;
        pracLogger.info("fact_lp()");
        
        for (long i = n; i > 1; --i)
            acc *= i;
        return acc;
    }
    
    private static long fact_iter(final long n, final long acc) {
    	return n > 1 ? fact_iter(n - 1, acc * n) : acc;
    }
    public static long fact_i(final long n) {return fact_iter(n, 1L);}
    
    public static float expt_lp(final float b, final float n) {
    	float acc = 1.0f;
    	
    	for (float i = n; 0.0f < i; --i)
    		acc *= b;
    	return acc;
    }
    
    private static float expt_iter(final float b, final float n, 
    		final float acc) {
    	return n > 0.0f ? expt_iter(b, n - 1, acc * b) : acc;
    }
    public static float expt_i(final float b, final float n) {
    	return expt_iter(b, n, 1.0f);
    }

	public static void main(String[] args) {
		System.out.format("fact(%d): %d\n", 5, fact_i(5));
	}
}
