// wasowski, Advanced Programming, IT University of Copenhagen
package fpinscala.laziness
import scala.language.higherKinds

import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import scala.util.Random

import stream00._    // uncomment to test the book solution (should pass your tests)
// import stream01._ // uncomment to test the broken headOption implementation
// import stream02._ // uncomment to test another version that breaks headOption

class StreamSpec
    extends org.scalatest.freespec.AnyFreeSpec
    with org.scalatest.matchers.should.Matchers
    with org.scalatestplus.scalacheck.ScalaCheckPropertyChecks {

  import Stream._

  // A simple converter of lists to streams

  def list2stream[A] (la: List[A]): Stream[A] =
    la.foldRight (Stream.empty[A]) (cons[A](_,_))

  // There  is  a name  clash  between  Stream.empty and  the  testing
  // library, so we need to qualify Stream.empty

  // An example generator  of random finite non-empty  streams (we use
  // the  built in  generator of  lists and  convert them  to streams,
  // using the above converter)
  //
  // 'suchThat'  filters  out  the  generated instances  that  do  not
  // satisfy the predicate given in the right argument.

  def genNonEmptyStream[A] (implicit arbA: Arbitrary[A]): Gen[Stream[A]] =
    for {
      la <- arbitrary[List[A]] suchThat { _.nonEmpty }
    } yield list2stream (la)

  "headOption" - {

    // Exercise 1 (no coding, understand)

    // A scenario test:

    "returns None on an empty Stream (01)" in {

      Stream.empty.headOption shouldBe (None)
    }


    // Two property tests:

    "returns the head of a singleton stream packaged in Some (02)" in {

      forAll { (n: Int) =>
        cons (n, Stream.empty).headOption should be (Some (n))
      }
    }

    "returns the head of random stream packaged in Some (02)" in {

      // Make the generator available in the context
      implicit val arbIntStream =
        Arbitrary[Stream[Int]] (genNonEmptyStream[Int])

      // Uses our generator of non empty streams
      // thanks to the implicit declaration above
      forAll { (s: Stream[Int]) =>
        s.headOption shouldNot be (None)
      }
    }

    // Exercise 2 (add here)
    //
    // ...
    "It should not force the tail of the stream" in {
      cons(1,throw new Exception("Exception 2")).headOption shouldBe Some(1)
    }
  }


  "take" - {
    def from (n: Int): Stream[Int] = cons(n,from(n+1))
    
    // Exercise 3
    "should not force any heads nor tails" in {
      val naturals: Stream[Int] = cons(1, Stream.empty)
      naturals.map(v=>throw new RuntimeException("Take is forcing head or tail")).take(3)
    }

    // Exercise 4
  "take(n) does not force the (n+1)st head ever" in {

   val naturals: Stream[Int] = from(1)
   val n = Random.between(1,100): Int
    naturals.append(naturals.map(v=>throw new RuntimeException("Take is forcing head or tail"))).take(n+1).forAll(_ >= 0)
  }
    // Exercise 5
  "s.take(n).take(n) ==s.take(n)" in {
     implicit val arbIntStream =
        Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
      //val n = Random.between(1,100): Int

      forAll { (s: Stream[Int],n:Int) =>
        s.take(n).take(n).toList shouldBe s.take(n).toList
      }
    //Stream(1,2,3).take(2).take(2).toList shouldBe Stream(1,2,3).take(2).toList
  }
  }

  "drop" - {
    
    // Exercise 6
    // Make the generator available in the context
      implicit val arbIntStream =
        Arbitrary[Stream[Int]] (genNonEmptyStream[Int])

      // Uses our generator of non empty streams
      // thanks to the implicit declaration above
      val n = Random.between(1,100): Int
      forAll { (s: Stream[Int]) =>
        s.drop(n).drop(n) shouldBe s.drop(n+n)
      }
    /* "s.drop(n).drop(m) ==s.drop(n+m) " in {
      val naturals: Stream[Int] = cons(1, Stream.empty)
      Stream(1,2,3,4,5,6).drop(2).drop(3).toList shouldBe Stream(1,2,3,4,5,6).drop(5).toList
     }*/
    // Exercise 7

  }


  "map" - {

     // Exercise 8
     //Test that x.map(id) ==x for any stream
     "x.map(id) ==x for any stream" in {
       // Make the generator available in the context
      implicit val arbIntStream =
        Arbitrary[Stream[Int]] (genNonEmptyStream[Int])

      // Uses our generator of non empty streams
      // thanks to the implicit declaration above
      /*
      s.map(x=> identity(x).toList should be (s.toList))
      */
      def f(n:Int):Int = n+1
      val n = Random.between(1,100): Int
      forAll { (s: Stream[Int]) =>
        s.map(f) == s
      }
     }

     // Exercise 9
    //Test that map terminates on infinite streams.
    "map terminates on infinite streams" in {
      def from (n: Int): Stream[Int] = cons(n,from(n+1))
      val naturals: Stream[Int] = from(1)
      naturals.map(x=>identity(x))

    }
  }

  "append" - {
    // Exercise 10

  }

}
