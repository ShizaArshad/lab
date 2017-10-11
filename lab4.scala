object lab4 {
	def lastNth(n:Int , list: List[Int]) = {
		def loop(iterator: Int , index:Int , sublist: List[Int]):Int = {
			if(iterator == index) sublist.head
			else loop(iterator+1 , index, sublist.tail)
		}
		val length:Int = list.length
		if(n>length) 0
		else loop(0 , length-n , list)
	}                                         
	
	
	def lastNthRecursive(n:Int , list: List[Int]) = {
		def loop(iterator: Int , index:Int , sublist: List[Int]):Int = {
			if(iterator == index) sublist.head
			else loop(iterator+1, index, sublist.tail)
		}
		val length:Int = list.length
		if(n>length) 0
		else loop(0, length-n, list)
	}                                         //> lastNthRecursive: (n: Int, list: List[Int])Int
	
	lastNth (2,List(1,1,2,2,3,3,5,5,8,9,10))  //> res0: Int = 9
	
	def abs(x:Int) = if(x<0) -x else x        // abs: (x: Int)Int
	
	def isPrime(num: Int):Boolean = {
		if(num == 0 || num == 1)
			false
		
		def loop(iterator:Int):Boolean = {
			if(iterator == abs(num)) true
			if(num%iterator == 0) false
			else loop(iterator+1)
		}
		loop(2)
	}                                         // isPrime: (num: Int)Boolean
	
	isPrime(5)                                // res1: Boolean = true
	
	def coprime(a:Int, b:Int):Int = if(b == 0) a else coprime(b, a%b)
                                                  // coprime: (a: Int, b: Int)Int
	coprime(25,100)                               // res2: Int = 25
	
	def XOR(a: Boolean, b: Boolean) = a != b  // XOR: (a: Boolean, b: Boolean)Boolean
	XOR(false, true)                          // res3: Boolean = true
	
	
	def AND(a: Boolean, b: Boolean) = if(a==true) b else false
                                                  // AND: (a: Boolean, b: Boolean)Boolean
	AND(false, false)                         // res4: Boolean = false
	
	def NAND(a: Boolean, b: Boolean) = if(a==true) !b else true
                                                  // NAND: (a: Boolean, b: Boolean)Boolean
	
	NAND(false, false)                        // res5: Boolean = true
	
	def function (a:Int , b:Int , c : Int , d :Int ,e :Int, x: Int) : Unit = {
	  
	}
	//lastNth(1, List(1,2,4,5))               //res6:Int = 2
}