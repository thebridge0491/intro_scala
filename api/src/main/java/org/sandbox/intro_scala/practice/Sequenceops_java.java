package org.sandbox.intro_scala.practice;

import java.util.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/** DocComment:
 * <p>Brief description.</p> */
public class Sequenceops_java {
	private static final Logger pracLogger = LoggerFactory.getLogger("prac");
	
	public static <T> void swapItems(int a, int b, List<T> lst) {
		T swap = lst.get(a); lst.set(a, lst.get(b)); lst.set(b, swap);
	}
	
	public static <T> int indexOf_lp(T data, List<T> lst,
			Comparator<? super T> cmp) {
		for (int i = 0; lst.size() > i; ++i)
			if (0 == cmp.compare(data, lst.get(i)))
				return i;
		return -1;
	}
	
	public static <T> void reverse_lp(List<T> lst) {
		int i = 0, j = lst.size() - 1;
		pracLogger.info("reverse_lp(List)");
		
		while (j > i)
			swapItems(i++, j--, lst);
	}
	
    
	public static <T> void swapItems(int a, int b, T[] arr) {
		T swap = arr[a]; arr[a] = arr[b]; arr[b] = swap;
	}
	
	public static <T> int indexOf_lp(T data, T[] arr,
			Comparator<? super T> cmp) {
		for (int i = 0; arr.length > i; ++i) {
			if (0 == cmp.compare(data, arr[i]))
				return i;
		}
		return -1;
	}
    
	public static <T> void reverse_lp(T[] arr) {
		int i = 0, j = arr.length - 1;
		pracLogger.info("reverse_lp()");
		
		while (j > i)
			swapItems(i++, j--, arr);
	}
	
	public static void main(String[] args) {
        StringBuilder strBldr = new StringBuilder();
        Integer[] ints = {0, 1, 2, 3};
        reverse_lp(ints);
        
        for (Integer ival : Arrays.asList(ints))
            strBldr.append(((0 < strBldr.length()) ? ", " : "") + ival);
        System.out.format("reverse([0, 1, 2, 3]): %s\n", strBldr.toString());
	}
}
