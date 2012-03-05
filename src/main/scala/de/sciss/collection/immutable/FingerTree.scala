package de.sciss.collection.immutable

/**
 * Variant of a finger tree which adds a measure.
 */
object FingerTree {
   // ---- Trees ----

   sealed trait FingerTree[ +V, +A ] {
      def isEmpty: Boolean

      def measure: V

      def headLeft: A
      def tailLeft: FingerTree[ V, A ]

      def headRight: A
      def tailRight: FingerTree[ V, A ]

      def +:[ B >: A ]( b: B ) : FingerTree[ V, B ]
      def :+[ B >: A ]( b: B ) : FingerTree[ V, B ]

      def viewLeft:  FTViewLeft[ FingerTree, A ]
      def viewRight: FTViewRight[ FingerTree, A ]

      def iterator: Iterator[ A ]
   }

   final case class Single[ V, A ]( a: A ) extends FingerTree[ V, A ] {
      def headLeft = a
      def tailLeft : FingerTree[ V, A ] = Empty

      def headRight = a
      def tailRight : FingerTree[ V, A ] = Empty

      def isEmpty = false

      def +:[ B >: A ]( b: B ) : FingerTree[ V, B ] = Deep( One( b ), Empty, One( a ))
      def :+[ B >: A ]( b: B ) : FingerTree[ V, B ] = Deep( One( a ), Empty, One( b ))

      def viewLeft  : FTViewLeft[  FingerTree, A ] = FTConsLeft[  FingerTree, A ]( a, Empty )
      def viewRight : FTViewRight[ FingerTree, A ] = FTConsRight[ FingerTree, A ]( Empty, a )

      def iterator : Iterator[ A ] = new Iterator[ A ] {
         var hasNext = true

         def next = {
            hasNext = false
            a
         }
      }

      override def toString = "FingerTree(Single(%s))".format(a)
   }

   final case class Deep[ V, A ]( prefix: Digit[ A ], tree: FingerTree[ V, Node[ A ]], suffix: Digit[ A ])
   extends FingerTree[ V, A ] {

      def isEmpty    = false

      val headLeft   = prefix.headLeft
      val headRight  = suffix.headRight

      def tailLeft  : FingerTree[ V, A ] = viewLeft.tail
      def tailRight : FingerTree[ V, A ] = viewRight.tail

      def +:[ B >: A ]( b: B ) : FingerTree[ V, B ] = prefix match {
         case Four( d, e, f, g ) => Deep( Two( b, d ), Node3( d, e, f ) +: tree, suffix )
         case partial            => Deep( b :: partial, tree, suffix )
      }

      def :+[ B >: A ]( b: B ) : FingerTree[ V, B ] = suffix match {
         case Four( g, f, e, d ) => Deep( prefix, tree :+ Node3( g, f, e ), Two( d, b ))
         case partial            => Deep( prefix, tree, partial + b )
      }

      def viewLeft : FTViewLeft[ FingerTree, A ] = {
         def deep( prefix: Digit[ A ], tree: FingerTree[ V, Node[ A ]], suffix: Digit[ A ]) = prefix match {
            case One( _ ) => tree.viewLeft match {
               case FTConsLeft( a, newTree ) => Deep( a.toDigit, newTree, suffix )
               case _                        => suffix.toTree[ V ]
            }

            case _prefix => Deep( _prefix.tailLeft, tree, suffix )
         }

         FTConsLeft( prefix.headLeft, deep( prefix, tree, suffix ))
      }

      def viewRight : FTViewLeft[ FingerTree, A ] = {
         def deep( prefix: Digit[ A ], tree: FingerTree[ V, Node[ A ]], suffix: Digit[ A ]) = suffix match {
            case One( _ ) => tree.viewRight match {
               case FTConsRight( newTree, a )   => Deep( prefix, newTree, a.toDigit )
               case _                           => prefix.toTree[ V ]
            }

            case _suffix => Deep( prefix, tree, _suffix.tailRight )
         }

         FTConsRight( deep( prefix, tree, suffix.tailRight ), suffix.headRight )
      }

      def iterator : Iterator[ A ] = prefix.iterator ++ (tree.iterator flatMap { _.toList.iterator }) ++ suffix.iterator

      override def toString = "FingerTree(%s, %s, %s)".format(prefix, tree, suffix)
   }

   case object Empty extends FingerTree[ Nothing, Nothing ] {
      val isEmpty = true

      def measure = sys.error( "Measure on an empty finger tree" )

      def headLeft = throw new NoSuchElementException("headLeft on empty finger tree")
      def tailLeft : FingerTree[ Nothing, Nothing ] = throw new NoSuchElementException("tailLeft on empty finger tree")

      def headRight = throw new NoSuchElementException("headRight on empty finger tree")
      def tailRight : FingerTree[ Nothing, Nothing ] = throw new NoSuchElementException("tailRight on empty finger tree")

      def +:[ B ]( b: B ) : FingerTree[ Nothing, B ] = Single( b )
      def :+[ B ]( b: B ) : FingerTree[ Nothing, B ] = Single( b )

      def viewLeft  : FTViewLeft[ FingerTree, Nothing ] = FTNilLeft[  FingerTree ]()
      def viewRight : FTViewLeft[ FingerTree, Nothing ] = FTNilRight[ FingerTree ]()

      def iterator : Iterator[ Nothing ] = new Iterator[Nothing] {
         val hasNext = false

         def next = throw new NoSuchElementException
      }

      override def toString = "FingerTree(Empty)"
   }

   // ---- Nodes ----

   sealed trait Node[ +A ] {
      def toDigit : Digit[ A ]
      def toList  : List[ A ]
   }

   final case class Node2[ A ]( a1: A, a2: A ) extends Node[ A ] {
      def toDigit : Digit[ A ] = Two(a1, a2)
      def toList  : List[ A ]  = List(a1, a2)

      override def toString = "Node2(%s, %s)".format(a1, a2)
   }

   final case class Node3[ A ]( a1: A, a2: A, a3: A ) extends Node[ A ] {
      def toDigit : Digit[ A ] = Three( a1, a2, a3 )
      def toList  : List[ A ]  = List(  a1, a2, a3 )

      override def toString = "Node3(%s, %s, %s)".format(a1, a2, a3)
   }

   // ---- Views ----

   sealed trait FTViewLeft[ +S[ +_ ], +A ] {
      def head : A
      def tail : S[ A ]
   }

   final case class FTConsLeft[ S[ _ ], A ]( head: A, tail: S[ A ]) extends FTViewLeft[ S, A ]

   final case class FTNilLeft[ S[ _ ]]() extends FTViewLeft[ S, Nothing ] {
      def head : Nothing      = throw new NoSuchElementException( "head on empty view" )
      def tail : S[ Nothing ] = throw new NoSuchElementException( "tail on empty view" )
   }

   sealed trait FTViewRight[ +S[ +_ ], +A ] {
      def tail : S[ A ]
      def head : A
   }

   final case class FTConsRight[ S[ _ ], A ]( tail: S[ A ], head: A ) extends FTViewRight[ S, A ]

   final case class FTNilRight[ S[ _ ]]() extends FTViewRight[ S, Nothing ] {
      def tail : Nothing      = throw new NoSuchElementException( "tail on empty view" )
      def head : S[ Nothing ] = throw new NoSuchElementException( "head on empty view" )
   }

   // ---- Digits ----

   sealed trait Digit[ +A ] {
      def headLeft  : A
      def tailLeft  : Digit[ A ]

      def headRight : A
      def tailRight : Digit[ A ]

      def ::[ B >: A ]( b: B ) : Digit[ B ]
      def +[  B >: A ]( b: B ) : Digit[ B ]

      def toTree[ V ]: FingerTree[ V, A ]

      def iterator: Iterator[ A ]
   }

   final case class One[ A ]( a1: A ) extends Digit[ A ] {
      def headLeft  = a1
      def tailLeft  : Digit[ A ] = throw new NoSuchElementException( "tail on digit: one" )

      def headRight = a1
      def tailRight : Digit[ A ] = throw new NoSuchElementException( "tail on digit: one" )

      def ::[ B >: A ]( b: B ) : Digit[ B ] = Two( b, a1 )
      def +[  B >: A ]( b: B ) : Digit[ B ] = Two( a1, b )

      def toTree[ V ] : FingerTree[ V, A ] = Single( a1 )

      def iterator : Iterator[ A ] = new Iterator[ A ] {
         var hasNext = true

         def next = {
            hasNext = false
            a1
         }
      }
   }

   final case class Two[ A ]( a1: A, a2: A ) extends Digit[ A ] {
      def headLeft  = a1
      def tailLeft  : Digit[ A ] = One( a2 )

      def headRight = a2
      def tailRight : Digit[ A ] = One(a1)

      def ::[ B >: A ]( b: B ) : Digit[ B ] = Three( b, a1, a2 )
      def +[  B >: A ]( b: B ) : Digit[ B ] = Three( a1, a2, b )

      def toTree[ V ] : FingerTree[ V, A ] = a1 +: Single( a2 )

      def iterator : Iterator[ A ] = (a1 :: a2 :: Nil).iterator
   }

   final case class Three[ A ]( a1: A, a2: A, a3: A ) extends Digit[ A ] {
      val headLeft  = a1
      def tailLeft  : Digit[ A ] = Two( a2, a3 )

      val headRight = a3
      def tailRight : Digit[ A ] = Two( a1, a2 )

      def ::[ B >: A ]( b: B ) : Digit[ B ] = Four( b, a1, a2, a3 )
      def +[  B >: A ]( b: B ) : Digit[ B ] = Four( a1, a2, a3, b )

      def toTree[ V ] : FingerTree[ V, A ] = a1 +: a2 +: Single(a3)

      def iterator : Iterator[ A ] = (a1 :: a2 :: a3 :: Nil).iterator
   }

   final case class Four[ A]( a1: A, a2: A, a3: A, a4: A ) extends Digit[ A ] {
      def headLeft  = a1
      def tailLeft  : Digit[ A ] = Three( a2, a3, a4 )

      def headRight = a4
      def tailRight : Digit[ A ] = Three( a1, a2, a3 )

      def ::[ B >: A ]( b: B ) = throw new UnsupportedOperationException( ":: on Four" )
      def +[  B >: A ]( b: B ) = throw new UnsupportedOperationException( "+ on Four" )

      def toTree[ V ] : FingerTree[ V, A ] = a1 +: a2 +: a3 +: Single( a4 )

      def iterator : Iterator[ A ] = (a1 :: a2 :: a3 :: a4 :: Nil).iterator
   }
}
