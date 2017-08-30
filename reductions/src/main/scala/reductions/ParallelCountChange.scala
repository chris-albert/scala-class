package reductions

import org.scalameter._
import common._

object ParallelCountChangeRunner {

  @volatile var seqResult = 0

  @volatile var parResult = 0

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 80,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val amount = 250
    val coins = List(1, 2, 5, 10, 20, 50)
    val seqtime = standardConfig measure {
      seqResult = ParallelCountChange.countChange(amount, coins)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential count time: $seqtime ms")

    def measureParallelCountChange(threshold: ParallelCountChange.Threshold): Unit = {
      val fjtime = standardConfig measure {
        parResult = ParallelCountChange.parCountChange(amount, coins, threshold)
      }
      println(s"parallel result = $parResult")
      println(s"parallel count time: $fjtime ms")
      println(s"speedup: ${seqtime / fjtime}")
    }

    measureParallelCountChange(ParallelCountChange.moneyThreshold(amount))
    measureParallelCountChange(ParallelCountChange.totalCoinsThreshold(coins.length))
    measureParallelCountChange(ParallelCountChange.combinedThreshold(amount, coins))
  }
}

object ParallelCountChange {


  private def countChangeGen(money: Int, coins: List[Int])(f: (Int,Int,List[Int],List[Int]) => Int): Int = {
    if(money < 0) 0
    else if (money == 0) 1
    else
      coins match {
        case Nil          => 0
        case head :: rest => f(money,head,coins,rest)
      }
  }

  /** Returns the number of ways change can be made from the specified list of
    * coins for the specified amount of money.
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    countChangeGen(money,coins)((money: Int, head: Int, coins: List[Int],rest: List[Int]) => {
      countChange(money - head,coins) + countChange(money,rest)
    })
  }

  type Threshold = (Int, List[Int]) => Boolean

  /** In parallel, counts the number of ways change can be made from the
    * specified list of coins for the specified amount of money.
    */
  def parCountChange(money: Int, coins: List[Int], threshold: Threshold): Int = {
    if(threshold(money,coins)) {
      countChange(money,coins)
    } else {
      countChangeGen(money,coins)((money: Int, head: Int, coins: List[Int],rest: List[Int]) => {
        val (a,b) = parallel(parCountChange(money - coins.head,coins,threshold),parCountChange(money,coins.tail,threshold))
        a + b
      })
    }
  }

  /** Threshold heuristic based on the starting money. */
  def moneyThreshold(startingMoney: Int): Threshold =
    (amount: Int, _) => amount <= (startingMoney * 2) / 3

  /** Threshold heuristic based on the total number of initial coins. */
  def totalCoinsThreshold(totalCoins: Int): Threshold =
    (_, coins: List[Int]) => coins.size <= (coins.length * 2) / 3


  /** Threshold heuristic based on the starting money and the initial list of coins. */
  def combinedThreshold(startingMoney: Int, allCoins: List[Int]): Threshold = {
    (amount: Int, coins: List[Int]) => amount * coins.size <= (startingMoney * allCoins.size) / 2
  }

  def scanLeft[A](inp: Array[A],
                  a0: A, f: (A, A) => A,
                  out: Array[A]): Unit = {
    out(0) = a0
    var a = a0
    var i = 0
    while (i < inp.length) {
      a = f(a, inp(i))
      i = i + 1
      out(i) = a
    }
  }

  def scanLeft2[A](inp: Array[A],
                   a0: A, f: (A, A) => A,
                   out: Array[A]): Unit = {
    val fi = { (i: Int, v: A) => reduceSeg1(inp, 0, i, a0, f) }
    mapSeg(inp, 0, inp.length, fi, out)
    val last = inp.length - 1
    out(last + 1) = f(out(last), inp(last))
  }

  def reduceSeg1[A](inp: Array[A], left: Int, right: Int,
                    a0: A, f: (A, A) => A): A = ???

  def mapSeg[A, B](inp: Array[A], left: Int, right: Int,
                   fi: (Int, A) => B,
                   out: Array[B]): Unit = ???

  sealed abstract class Tree[A]

  case class Leaf[A](a: A) extends Tree[A]

  case class Node[A](l: Tree[A], r: Tree[A]) extends Tree[A]

  def prepend[A](x: A, t: Tree[A]): Tree[A] = t match {
    case Leaf(v) => Node(Leaf(x), Leaf(v))
    case Node(l, r) => Node(prepend(x, l), r)
  }

  sealed abstract class TreeRes[A] {
    val res: A
  }

  case class LeafRes[A](override val res: A) extends TreeRes[A]

  case class NodeRes[A](l: TreeRes[A],
                        override val res: A,
                        r: TreeRes[A]) extends TreeRes[A]

  def reduceRes[A](t: Tree[A], f: (A, A) => A): TreeRes[A] = t match {
    case Leaf(v) => LeafRes(v)
    case Node(l, r) => {
      val (tL, tR) = (reduceRes(l, f), reduceRes(r, f))
      NodeRes(tL, f(tL.res, tR.res), tR)
    }
  }

  def upsweep[A](t: Tree[A], f: (A, A) => A): TreeRes[A] = t match {
    case Leaf(v) => LeafRes(v)
    case Node(l, r) => {
      val (tL, tR) = parallel(upsweep(l, f), upsweep(r, f))
      NodeRes(tL, f(tL.res, tR.res), tR)
    }
  }

  def downsweep[A](t: TreeRes[A], a0: A, f: (A, A) => A): Tree[A] = t match {
    case LeafRes(a) => Leaf(f(a0, a))
    case NodeRes(l, _, r) => {
      val (tL, tR) = parallel(downsweep[A](l, a0, f), downsweep[A](r, f(a0, l.res), f))
      Node(tL, tR)
    }
  }

  def scanLeft[A](t: Tree[A], a0: A, f: (A, A) => A): Tree[A] = {
    val tRes = upsweep(t, f)
    val scan1 = downsweep(tRes, a0, f)
    prepend(a0, scan1)
  }

  sealed abstract class TreeResA[A] {
    val res: A
  }

  case class LeafResA[A](from: Int, to: Int, override val res: A) extends TreeResA[A]

  case class NodeResA[A](l: TreeResA[A],
                         override val res: A,
                         r: TreeRes[A]) extends TreeResA[A]

}
