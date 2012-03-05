package de.sciss.fingertree

import com.codecommit.collection.FingerQueue

object TestCodeCommit extends App {
   val N    = 50000 // 100000
   val SEED = 0L
   val WARM = 2

   def t()  = System.currentTimeMillis()

   def testFill() : Long = {
      var q    = FingerQueue.empty[ Int ]
      val rnd  = new util.Random( SEED )
      val t1   = t()
      var i = 0; while( i < N ) {
         q = q.enqueue( rnd.nextInt( 1000 ))
      i += 1 }
      val t2   = t()
      assert( q.size == N )
      t2 - t1
   }

   var r = 0; while( r < WARM ) {
      testFill()
   r += 1 }

   val perfTestFill = testFill()
   println( "Test fill with N = " + N + " (warm up = " + WARM + ") took " + perfTestFill + " ms." )
}
