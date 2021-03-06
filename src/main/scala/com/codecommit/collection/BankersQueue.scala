package com.codecommit.collection

import scala.collection.{generic, immutable, mutable, LinearSeqLike}
import generic.{CanBuildFrom, GenericCompanion, GenericTraversableTemplate, SeqFactory}
import immutable.LinearSeq
import mutable.{Builder, ListBuffer}

/**
 * An implementation of Okasaki's fast persistent Banker's Queue data structure
 * (provides a better constant factor than the default {@link scala.collection.immutable.Queue}).
 */
class BankersQueue[+A] private (private val fsize: Int, private val front: Stream[A], private val rsize: Int, private val rear: List[A])
    extends LinearSeq[A]
    with GenericTraversableTemplate[A, BankersQueue]
    with LinearSeqLike[A, BankersQueue[A]] {

  override val companion: GenericCompanion[BankersQueue] = BankersQueue

  val length: Int = fsize + rsize

  override def isEmpty = length == 0

  override def head: A = front match {
    case hd #:: _ => hd
    case _ => throw new NoSuchElementException("head on empty queue")
  }

  override def tail: BankersQueue[A] = front match {
    case _ #:: tail => check(new BankersQueue(fsize - 1, tail, rsize, rear))
    case _ => throw new NoSuchElementException("tail on empty queue")
  }

  def apply(i: Int) = {
    if (i < fsize)
      front(i)
    else if (i < fsize + rsize)
      rear(rsize - (i - fsize) - 1)
    else {
      assert(i > length)
      throw new NoSuchElementException("index out of range: " + i)
    }
  }

  def +[B >: A](b: B) = enqueue(b)

  def enqueue[B >: A](b: B) = check(new BankersQueue(fsize, front, rsize + 1, b :: rear))

  def dequeue: (A, BankersQueue[A]) = front match {
    case hd #:: tail => (hd, check(new BankersQueue(fsize - 1, tail, rsize, rear)))
    case _ => throw new NoSuchElementException("dequeue on empty queue")
  }

  override def iterator = (front ++ rear.reverse).iterator         // force for traversal

  private def check[B](q: BankersQueue[B]) = {
    if (q.rsize <= q.fsize)
      q
    else
      new BankersQueue(q.fsize + q.rsize, q.front ++ q.rear.reverse, 0, Nil)
  }
}

object BankersQueue extends SeqFactory[BankersQueue] {
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, BankersQueue[A]] = new GenericCanBuildFrom[A]

  def newBuilder[A]: Builder[A, BankersQueue[A]] =
    new ListBuffer[A] mapResult { x => new BankersQueue[A](x.length, x.reverse.toStream, 0, Nil) }

  override def empty[A]: BankersQueue[A] = new BankersQueue[A](0, Stream(), 0, Nil)

  override def apply[A](xs: A*): BankersQueue[A] = new BankersQueue[A](xs.length, xs.reverse.toStream, 0, Nil)
}
