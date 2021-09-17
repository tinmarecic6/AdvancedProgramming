// Advanced Programming, Exercises by A. Wąsowski, IT University of Copenhagen
//
// Work on this file by following the associated exercise sheet
// (available in PDF in the same directory).
//
// The file is meant to be compiled inside sbt, using the 'compile' command.
// To run the compiled file use the 'run' or 'runMain'.
// To load the file int the REPL use the 'console' command.
//
// Continue solving exercises in the order presented in the PDF file. The file
// shall always compile, run, and pass tests (for the solved exercises),
// after you are done with each exercise (if you do them in order).
// Compile and test frequently. Best continously.

// The extension of App allows writing the main method statements at the top
// level (the so called default constructor). For App objects they will be
// executed as if they were placed in the main method in Java.

package fpinscala

object Exercises extends App with ExercisesInterface {

  import fpinscala.List._

  // Exercise 3

  def fib (n: Int): Int = if (n==1) 0 else if(n==2) 1 else fib(n-1) + fib(n-2)

  //tail-recursion version
  def fib_tail_rec(n:Int):Int ={  
    def fib_tail(n: Int, a: Int, b: Int): Int ={
      //println(n)
      if (n==0) 0
      else if (n==1) b
      else fib_tail(n - 1, b, a + b)
  }
   fib_tail(n, 0 , 1)
  }

  // Exercise 4
//kinda works, dont know how to call function with array as argument
  def isSorted[A] (as: Array[A], comparison: (A,A) =>  Boolean): Boolean = {
     //@scala.annotation.tailrec
     def iterator(as:Array[A],a:Int,b:Int):Boolean = {
       if (as.size == 0 || as.size==1) true else
          if (b+1<as.size)
            if (comparison(as(a),as(b))) iterator(as,b,b+1) else false
          else true
     }
  iterator(as,0,1)
  }

  // Exercise 5

  def curry[A,B,C] (f: (A,B)=>C): A => (B => C) = ???

  // Exercise 6

  def uncurry[A,B,C] (f: A => B => C): (A,B) => C = ???

  // Exercise 7

  def compose[A,B,C] (f: B => C, g: A => B) : A => C = ???

//val myList = Array(6,2,3,4,5,6)
//println(isSorted(Array(6,2,3,4,5,6)))
}
