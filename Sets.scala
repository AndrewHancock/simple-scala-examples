object Sets {
	def main(args: Array[String]) {
		val setA = Set(1, 2, 3, 4);
		val setB = Set(3, 4, 5, 6);
		
		println("A: " + setA);
		println("B: " + setB);
		
		// Ownership: two forms
		// Both test only for an individual element, not a subset.
		println("A contains 2: " + (setA contains 2)); // Note parens
		println("B contains 1: " + setB(1));
		
		println("B subset of A: " + (setB subsetOf setA));
		println("Set(1, 2, 3) subset of A: " + (Set(1, 2, 3) subsetOf setA));
		
		//+ operator adds a single element OR a Tuple
		//+ cannot be used to union a set
		println("A + 10: " + (setA + 10)); 
		println("A + (10, 11, 12): " + (setA + (10, 11, 12)));
		//++ operator evaluates to the union of two sets
		println("A ++ B: " + (setA ++ setB));
		
		//- and -- perform minus operations of elements, tuples and sets
		
		// Intersection
		println("A & B: " + (setA & setB));
		// Union
		println("A | B: " + (setA | setB));
		
		
		
		
		
		
		
		
		
		
	}
}