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

  // Exercise 4

  def isSorted[A] (as: Array[A], comparison: (A,A) =>  Boolean): Boolean = {
    def iterator(as:Array[A], a:Int, b:Int):Boolean={
      if(as.size == 0 || as.size==1) true else
        if(b+1<=as.size)
          if(comparison(as(a),as(b))) iterator(as,b,b+1) else false
        else
          true
    }
    iterator(as,0,1)
  } 

  // Exercise 5

  def curry[A,B,C] (f: (A,B)=>C): A => (B => C) = (a:A)=>((b:B)=>f(a,b))

  // Exercise 6

  def uncurry[A,B,C] (f: A => B => C): (A,B) => C = (a:A,b:B)=>f(a:A)(b:B)

  // Exercise 7

  def compose[A,B,C] (f: B => C, g: A => B) : A => C = (a:A) => f(g(a))

}
