/*
 * FingerTree.scala
 * (FingerTree)
 *
 * Copyright (c) 2012 Hanns Holger Rutz. All rights reserved.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 *
 *
 * For further information, please contact Hanns Holger Rutz at
 * contact@sciss.de
 */

package de.sciss.collection.immutable

/**
 * Variant of a finger tree which adds a measure.
 */
object FingerTree {
   def empty[ V, A ]( implicit m: Measure[ Nothing, V ]) : FingerTree[ V, A ] = new Empty[ V ]( m.zero )

   // ---- Trees ----

   sealed trait FingerTree[ V, +A ] {
      def isEmpty: Boolean

      def measure : V

      def headLeft: A
      def tailLeft( implicit m: Measure[ A, V ]): FingerTree[ V, A ]

      def headRight: A
      def tailRight( implicit m: Measure[ A, V ]): FingerTree[ V, A ]

      def +:[ A1 >: A ]( a1: A1 )( implicit m: Measure[ A1, V ]) : FingerTree[ V, A1 ]
      def :+[ A1 >: A ]( a1: A1 )( implicit m: Measure[ A1, V ]) : FingerTree[ V, A1 ]

      def viewLeft( implicit m: Measure[ A, V ]): ViewLeft[ V, A ]
      def viewRight( implicit m: Measure[ A, V ]): ViewRight[ V, A ]

      def iterator: Iterator[ A ]
   }

   final private case class Single[ V, A ]( measure: V, a: A ) extends FingerTree[ V, A ] {
      def headLeft = a
      def tailLeft( implicit m: Measure[ A, V ]) : FingerTree[ V, A ] = empty[ V, A ]

      def headRight = a
      def tailRight( implicit m: Measure[ A, V ]) : FingerTree[ V, A ] = empty[ V, A ]

      def isEmpty = false

      def +:[ A1 >: A ]( b: A1 )( implicit m: Measure[ A1, V ]) : FingerTree[ V, A1 ] = {
         val vPrefix = m.unit( b )
         val prefix  = One( vPrefix, b )
         val vSuffix = m.unit( a )
         val suffix = One( vSuffix, a )
         Deep( m.|+|( vPrefix, vSuffix ), prefix, empty[ V, Node[ A1 ]], suffix )
      }

      def :+[ A1 >: A ]( b: A1 )( implicit m: Measure[ A1, V ]) : FingerTree[ V, A1 ] = {
         val vPrefix = m.unit( a )
         val prefix  = One( vPrefix, a )
         val vSuffix = m.unit( b )
         val suffix  = One( vSuffix, b )
         Deep( m.|+|( vPrefix, vSuffix ), prefix, empty[ V, Node[ A1 ]], suffix )
      }

      def viewLeft( implicit m: Measure[ A, V ]) : ViewLeft[  V, A ] = ViewConsLeft[  V, A ]( a, empty[ V, A ])
      def viewRight( implicit m: Measure[ A, V ]) : ViewRight[ V, A ] = ViewConsRight[ V, A ]( empty[ V, A ], a )

      def iterator : Iterator[ A ] = new Iterator[ A ] {
         var hasNext = true

         def next = {
            hasNext = false
            a
         }
      }

      override def toString = "FingerTree(Single(%s))".format(a)
   }

   final private case class Deep[ V, A ]( measure: V, prefix: Digit[ V, A ], tree: FingerTree[ V, Node[ A ]], suffix: Digit[ V, A ])
   extends FingerTree[ V, A ] {

      def isEmpty    = false

      val headLeft   = prefix.headLeft
      val headRight  = suffix.headRight

      def tailLeft( implicit m: Measure[ A, V ]) : FingerTree[ V, A ] = viewLeft.tail
      def tailRight( implicit m: Measure[ A, V ]) : FingerTree[ V, A ] = viewRight.tail

      def +:[ A1 >: A ]( b: A1 )( implicit m: Measure[ A1, V ]) : FingerTree[ V, A1 ] = {
         val vb   = m.unit( b )
         val vNew = m.|+|( vb, measure )
         prefix match {
            case Four( _, d, e, f, g ) =>
               implicit def m1: Measure[ Node[ A1 ], V ] = sys.error( "TODO" )
               val prefix  = Two( m.|+|( vb, m.unit( d )), b, d )
               val treeNew = tree.+:[ Node[ A1 ]]( Node3( d, e, f ))
               Deep( vNew, prefix, treeNew, suffix )

            case partial =>
               Deep( vNew, b +: partial, tree, suffix )
         }
      }

      def :+[ A1 >: A ]( b: A1 )( implicit m: Measure[ A1, V ]) : FingerTree[ V, A1 ] = {
         val vb   = m.unit( b )
         val vNew = m.|+|( vb, measure )
         suffix match {
            case Four( _, g, f, e, d ) =>
               implicit def m1: Measure[ Node[ A1 ], V ] = sys.error( "TODO" )
               val treeNew = tree.:+[ Node[ A1 ]]( Node3( g, f, e ))
               val suffix  = Two( m.|+|( m.unit( d ), vb ), d, b )
               Deep( vNew, prefix, treeNew, suffix )
            case partial =>
               Deep( vNew, prefix, tree, partial :+ b )
         }
      }

      def viewLeft( implicit m: Measure[ A, V ]) : ViewLeft[ V, A ] = {
         implicit def m1: Measure[ Node[ A ], V ] = sys.error( "TODO" )
         def deep( prefix: Digit[ V, A ], tree: FingerTree[ V, Node[ A ]], suffix: Digit[ V, A ]) = prefix match {
            case One( _, _ ) => tree.viewLeft match {
               case ViewConsLeft( a, newTree ) =>
                  val prefixNew = a.toDigit[ V ]
                  val vNew = m.|+|( m.|+|( prefixNew.measure, newTree.measure ), suffix.measure )
                  Deep( vNew, prefixNew, newTree, suffix )
               case _ =>
                  suffix.toTree
            }

            case _prefix =>
               val prefixNew = _prefix.tailLeft
               val vNew = m.|+|( m.|+|( prefixNew.measure, tree.measure ), suffix.measure )
               Deep( vNew, prefixNew, tree, suffix )
         }

         ViewConsLeft( prefix.headLeft, deep( prefix, tree, suffix ))
      }

      def viewRight( implicit m: Measure[ A, V ]) : ViewRight[ V, A ] = {
         implicit def m1: Measure[ Node[ A ], V ] = sys.error( "TODO" )
         def deep( prefix: Digit[ V, A ], tree: FingerTree[ V, Node[ A ]], suffix: Digit[ V, A ]) = suffix match {
            case One( _, _ ) => tree.viewRight match {
               case ViewConsRight( newTree, a ) =>
                  val suffixNew = a.toDigit[ V ]
                  val vNew = m.|+|( m.|+|( prefix.measure, newTree.measure ), suffixNew.measure )
                  Deep( vNew, prefix, newTree, suffixNew )
               case _ =>
                  prefix.toTree
            }

            case _suffix =>
               val suffixNew = _suffix.tailRight
               val vNew = m.|+|( m.|+|( prefix.measure, tree.measure ), suffixNew.measure )
               Deep( vNew, prefix, tree, suffixNew )
         }

         ViewConsRight( deep( prefix, tree, suffix.tailRight ), suffix.headRight )
      }

      def iterator : Iterator[ A ] = prefix.iterator ++ (tree.iterator flatMap { _.toList.iterator }) ++ suffix.iterator

      override def toString = "FingerTree(%s, %s, %s)".format(prefix, tree, suffix)
   }

   final case class Empty[ V ]( measure: V ) extends FingerTree[ V, Nothing ] {
      val isEmpty = true

      def headLeft = throw new NoSuchElementException("headLeft on empty finger tree")
      def tailLeft( implicit m: Measure[ Nothing, V ]) : FingerTree[ V, Nothing ] = throw new NoSuchElementException("tailLeft on empty finger tree")

      def headRight = throw new NoSuchElementException("headRight on empty finger tree")
      def tailRight( implicit m: Measure[ Nothing, V ]) : FingerTree[ V, Nothing ] = throw new NoSuchElementException("tailRight on empty finger tree")

      def +:[ A1 ]( a1: A1 )( implicit m: Measure[ A1, V ]) : FingerTree[ V, A1 ] = Single( m.unit( a1 ), a1 )
      def :+[ A1 ]( a1: A1 )( implicit m: Measure[ A1, V ]) : FingerTree[ V, A1 ] = Single( m.unit( a1 ), a1 )

      def viewLeft(  implicit m: Measure[ Nothing, V ]) : ViewLeft[  V, Nothing ] = ViewNilLeft[  V ]()
      def viewRight( implicit m: Measure[ Nothing, V ]) : ViewRight[ V, Nothing ] = ViewNilRight[ V ]()

      def iterator : Iterator[ Nothing ] = new Iterator[ Nothing ] {
         val hasNext = false

         def next = throw new NoSuchElementException
      }

      override def toString = "FingerTree(Empty)"
   }

   // ---- Nodes ----

   sealed trait Node[ +A ] {
      def toDigit[ V ]( implicit m: Measure[ A, V ]) : Digit[ V, A ]
      def toList  : List[ A ]
   }

   final case class Node2[ A ]( a1: A, a2: A ) extends Node[ A ] {
      def toDigit[ V ]( implicit m: Measure[ A, V ]) : Digit[ V, A ] =
         Two( m.|+|( m.unit( a1 ), m.unit( a2 )), a1, a2 )

      def toList : List[ A ]  = List( a1, a2 )

      override def toString = "Node2(%s, %s)".format(a1, a2)
   }

   final case class Node3[ A ]( a1: A, a2: A, a3: A ) extends Node[ A ] {
      def toDigit[ V ]( implicit m: Measure[ A, V ]) : Digit[ V, A ] =
         Three( m.|+|( m.|+|( m.unit( a1 ), m.unit( a2 )), m.unit( a3 )), a1, a2, a3 )

      def toList : List[ A ]  = List(  a1, a2, a3 )

      override def toString = "Node3(%s, %s, %s)".format(a1, a2, a3)
   }

   // ---- Views ----

   sealed trait ViewLeft[ V, +A ] {
      def head : A
      def tail : FingerTree[ V, A ]
   }

   final case class ViewConsLeft[ V, A ]( head: A, tail: FingerTree[ V, A ]) extends ViewLeft[ V, A ]

   final case class ViewNilLeft[ V ]() extends ViewLeft[ V, Nothing ] {
      def head : Nothing                  = throw new NoSuchElementException( "head on empty view" )
      def tail : FingerTree[ V, Nothing ] = throw new NoSuchElementException( "tail on empty view" )
   }

   sealed trait ViewRight[ V, +A ] {
      def tail : FingerTree[ V, A ]
      def head : A
   }

   final case class ViewConsRight[ V, A ]( tail: FingerTree[ V, A ], head: A ) extends ViewRight[ V, A ]

   final case class ViewNilRight[ V ]() extends ViewRight[ V, Nothing ] {
      def tail : FingerTree[ V, Nothing ] = throw new NoSuchElementException( "tail on empty view" )
      def head : Nothing                  = throw new NoSuchElementException( "head on empty view" )
   }

   // ---- Digits ----

   sealed trait Digit[ V, +A ] {
      def measure: V

      def headLeft  : A
      def tailLeft( implicit m: Measure[ A, V ]) : Digit[ V, A ]

      def headRight : A
      def tailRight( implicit m: Measure[ A, V ]) : Digit[ V, A ]

      def +:[ A1 >: A ]( b: A1 )( implicit m: Measure[ A1, V ]) : Digit[ V, A1 ]
      def :+[ A1 >: A ]( b: A1 )( implicit m: Measure[ A1, V ]) : Digit[ V, A1 ]

      def toTree( implicit m: Measure[ A, V ]): FingerTree[ V, A ]

      def iterator: Iterator[ A ]
   }

   final case class One[ V, A ]( measure: V, a1: A ) extends Digit[ V, A ] {
      def headLeft  = a1
      def tailLeft( implicit m: Measure[ A, V ]) : Digit[ V, A ] = throw new NoSuchElementException( "tail on digit: one" )

      def headRight = a1
      def tailRight( implicit m: Measure[ A, V ]) : Digit[ V, A ] = throw new NoSuchElementException( "tail on digit: one" )

      def +:[ A1 >: A ]( b: A1 )( implicit m: Measure[ A1, V ]) : Digit[ V, A1 ] = Two( m.|+|( m.unit( b ), measure ), b, a1 )
      def :+[ A1 >: A ]( b: A1 )( implicit m: Measure[ A1, V ]) : Digit[ V, A1 ] = Two( m.|+|( measure, m.unit( b )),  a1, b )

      def toTree( implicit m: Measure[ A, V ]) : FingerTree[ V, A ] = Single( measure, a1 )

      def iterator : Iterator[ A ] = new Iterator[ A ] {
         var hasNext = true

         def next = {
            hasNext = false
            a1
         }
      }
   }

   final case class Two[ V, A ]( measure: V, a1: A, a2: A ) extends Digit[ V, A ] {
      def headLeft  = a1
      def tailLeft( implicit m: Measure[ A, V ]) : Digit[ V, A ] = One( m.unit( a2 ), a2 )

      def headRight = a2
      def tailRight( implicit m: Measure[ A, V ]) : Digit[ V, A ] = One( m.unit( a1 ), a1 )

      def +:[ A1 >: A ]( b: A1 )( implicit m: Measure[ A1, V ]) : Digit[ V, A1 ] = Three( m.|+|( m.unit( b ), measure ), b, a1, a2 )
      def :+[ A1 >: A ]( b: A1 )( implicit m: Measure[ A1, V ]) : Digit[ V, A1 ] = Three( m.|+|( measure, m.unit( b )),  a1, a2, b )

      def toTree( implicit m: Measure[ A, V ]) : FingerTree[ V, A ] = a1 +: Single( m.unit( a2 ), a2 )

      def iterator : Iterator[ A ] = (a1 :: a2 :: Nil).iterator
   }

   final case class Three[ V, A ]( measure: V, a1: A, a2: A, a3: A ) extends Digit[ V, A ] {
      val headLeft  = a1
      def tailLeft( implicit m: Measure[ A, V ]) : Digit[ V, A ] = Two( m.|+|( m.unit( a2 ), m.unit( a3 )), a2, a3 )

      val headRight = a3
      def tailRight( implicit m: Measure[ A, V ]) : Digit[ V, A ] = Two( m.|+|( m.unit( a1 ), m.unit( a2 )), a1, a2 )

      def +:[ A1 >: A ]( b: A1 )( implicit m: Measure[ A1, V ]) : Digit[ V, A1 ] =
         Four( m.|+|( m.unit( b ), measure ), b, a1, a2, a3 )

      def :+[ A1 >: A ]( b: A1 )( implicit m: Measure[ A1, V ]) : Digit[ V, A1 ] =
         Four( m.|+|( measure, m.unit( b )), a1, a2, a3, b )

      def toTree( implicit m: Measure[ A, V ]) : FingerTree[ V, A ] = a1 +: a2 +: Single( m.unit( a3 ), a3 )

      def iterator : Iterator[ A ] = (a1 :: a2 :: a3 :: Nil).iterator
   }

   final case class Four[ V, A ]( measure: V, a1: A, a2: A, a3: A, a4: A ) extends Digit[ V, A ] {
      def headLeft  = a1
      def tailLeft( implicit m: Measure[ A, V ]) : Digit[ V, A ] =
         Three( m.|+|( m.|+|( m.unit( a2 ), m.unit( a3 )), m.unit( a4 )), a2, a3, a4 )

      def headRight = a4
      def tailRight( implicit m: Measure[ A, V ]) : Digit[ V, A ] =
         Three( m.|+|( m.|+|( m.unit( a1 ), m.unit( a2 )), m.unit( a3 )), a1, a2, a3 )

      def +:[ A1 >: A ]( b: A1 )( implicit m: Measure[ A1, V ]) = throw new UnsupportedOperationException( ":: on Four" )
      def :+[ A1 >: A ]( b: A1 )( implicit m: Measure[ A1, V ]) = throw new UnsupportedOperationException( "+ on Four" )

      def toTree( implicit m: Measure[ A, V ]) : FingerTree[ V, A ] = a1 +: a2 +: a3 +: Single( m.unit( a4 ), a4 )

      def iterator : Iterator[ A ] = (a1 :: a2 :: a3 :: a4 :: Nil).iterator
   }
}