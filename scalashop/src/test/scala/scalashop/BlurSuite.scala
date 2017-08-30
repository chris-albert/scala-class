package scalashop

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import scala.util.Random

@RunWith(classOf[JUnitRunner])
class BlurSuite extends FunSuite {
  test("boxBlurKernel should correctly handle radius 0") {
    val src = new Img(5, 5)

    for (x <- 0 until 5; y <- 0 until 5)
      src(x, y) = rgba(x, y, x + y, math.abs(x - y))

    for (x <- 0 until 5; y <- 0 until 5)
      assert(boxBlurKernel(src, x, y, 0) === rgba(x, y, x + y, math.abs(x - y)),
        "boxBlurKernel(_,_,0) should be identity.")
  }

  test("boxBlurKernel should return the correct value on an interior pixel " +
    "of a 3x4 image with radius 1") {
    val src = new Img(3, 4)
    src(0, 0) = 0
    src(1, 0) = 1
    src(2, 0) = 2
    src(0, 1) = 3
    src(1, 1) = 4
    src(2, 1) = 5
    src(0, 2) = 6
    src(1, 2) = 7
    src(2, 2) = 8
    src(0, 3) = 50
    src(1, 3) = 11
    src(2, 3) = 16

    assert(boxBlurKernel(src, 1, 2, 1) === 12,
      s"(boxBlurKernel(1, 2, 1) should be 12, " +
        s"but it's ${boxBlurKernel(src, 1, 2, 1)})")
  }

  test("radiusCoords should return coords for radius 0") {
    assert(radiusCoords(1, 1, 0) == Seq((1, 1)))
  }

  test("radiusCoords should return coords for radius 1") {
    assert(radiusCoords(1, 1, 1) == Seq(
      (0, 0), (1, 0), (2, 0), (0, 1), (1, 1), (2, 1), (0, 2), (1, 2), (2, 2)
    ))
  }

  test("radiusCoords should return coords for radius 3") {
    assert(radiusCoords(1, 1, 3) == Seq(
      (-2, -2), (-1, -2), (0, -2), (1, -2), (2, -2), (3, -2), (4, -2),
      (-2, -1), (-1, -1), (0, -1), (1, -1), (2, -1), (3, -1), (4, -1),
      (-2, 0), (-1, 0), (0, 0), (1, 0), (2, 0), (3, 0), (4, 0),
      (-2, 1), (-1, 1), (0, 1), (1, 1), (2, 1), (3, 1), (4, 1),
      (-2, 2), (-1, 2), (0, 2), (1, 2), (2, 2), (3, 2), (4, 2),
      (-2, 3), (-1, 3), (0, 3), (1, 3), (2, 3), (3, 3), (4, 3),
      (-2, 4), (-1, 4), (0, 4), (1, 4), (2, 4), (3, 4), (4, 4)
    ))
  }

  test("validRadiusCoords should return coords for top left") {
    assert(validRadiusCoords(0, 0, 1, new Img(3, 3)) == Seq(
      (0, 0),
      (1, 0),
      (0, 1),
      (1, 1)
    ))
  }

  test("validRadiusCoords should return coords for bottom right") {
    assert(validRadiusCoords(2, 2, 1, new Img(3, 3)) == Seq(
      (1, 1),
      (2, 1),
      (1, 2),
      (2, 2)
    ))
  }

  test("validRadiusCoords should return coords for bottom odd size") {
    assert(validRadiusCoords(3, 2, 1, new Img(4, 3)) == Seq(
      (2, 1),
      (3, 1),
      (2, 2),
      (3, 2)
    ))
  }

  test("HorizontalBoxBlur.blur with radius 1 should correctly blur the entire 3x3 image") {
    val w = 3
    val h = 3
    val src = new Img(w, h)
    val dst = new Img(w, h)
    src(0, 0) = 0
    src(1, 0) = 1
    src(2, 0) = 2
    src(0, 1) = 3
    src(1, 1) = 4
    src(2, 1) = 5
    src(0, 2) = 6
    src(1, 2) = 7
    src(2, 2) = 8

    HorizontalBoxBlur.blur(src, dst, 0, 2, 1)

    def check(x: Int, y: Int, expected: Int) =
      assert(dst(x, y) == expected,
        s"(destination($x, $y) should be $expected)")

    check(0, 0, 2)
    check(1, 0, 2)
    check(2, 0, 3)
    check(0, 1, 3)
    check(1, 1, 4)
    check(2, 1, 4)
    check(0, 2, 0)
    check(1, 2, 0)
    check(2, 2, 0)
  }

  test("VerticalBoxBlur.blur with radius 2 should correctly blur the entire " +
    "4x3 image") {
    val w = 4
    val h = 3
    val src = new Img(w, h)
    val dst = new Img(w, h)
    src(0, 0) = 0
    src(1, 0) = 1
    src(2, 0) = 2
    src(3, 0) = 9
    src(0, 1) = 3
    src(1, 1) = 4
    src(2, 1) = 5
    src(3, 1) = 10
    src(0, 2) = 6
    src(1, 2) = 7
    src(2, 2) = 8
    src(3, 2) = 11

    VerticalBoxBlur.blur(src, dst, 0, 4, 2)

    def check(x: Int, y: Int, expected: Int) =
      assert(dst(x, y) == expected,
        s"(destination($x, $y) should be $expected)")

    check(0, 0, 4)
    check(1, 0, 5)
    check(2, 0, 5)
    check(3, 0, 6)
    check(0, 1, 4)
    check(1, 1, 5)
    check(2, 1, 5)
    check(3, 1, 6)
    check(0, 2, 4)
    check(1, 2, 5)
    check(2, 2, 5)
    check(3, 2, 6)
  }

  test("VerticalBoxBlur.splitTasks should split all correctly") {
    assert(VerticalBoxBlur.splitTasks(3, 10) == Seq(
      (0, 2),
      (3, 5),
      (6, 9)
    ))

    assert(VerticalBoxBlur.splitTasks(4, 10) == Seq(
      (0, 1), (2, 3), (4, 6), (7, 9)
    ))

    assert(VerticalBoxBlur.splitTasks(2, 10) == Seq(
      (0, 4), (5, 9)
    ))
  }

  test("HorizontalBoxBlur.blur with radius 2 should correctly blur first row") {

    val w = 4
    val h = 3
    val src = new Img(w, h)
    val dst = new Img(w, h)
    src(0, 0) = 1
    src(1, 0) = 2
    src(2, 0) = 3
    src(3, 0) = 4
    src(0, 1) = 5
    src(1, 1) = 6
    src(2, 1) = 7
    src(3, 1) = 8
    src(0, 2) = 9
    src(1, 2) = 10
    src(2, 2) = 11
    src(3, 2) = 12

    HorizontalBoxBlur.blur(src, dst, 0, 1, 2)

    def check(x: Int, y: Int, expected: Int) =
      assert(dst(x, y) == expected,
        s"(destination($x, $y) should be $expected)")

    check(0, 0, 6)
    check(1, 0, 6)
    check(2, 0, 6)
    check(3, 0, 7)
    check(0, 1, 0)
    check(1, 1, 0)
    check(2, 1, 0)
    check(3, 1, 0)
    check(0, 2, 0)
    check(1, 2, 0)
    check(2, 2, 0)
    check(3, 2, 0)

  }

  test("HorizontalBoxBlur.blur with radius 2 should correctly blur the entire 4x3 image") {
    val w = 4
    val h = 3
    val src = new Img(w, h)
    val dst = new Img(w, h)
    src(0, 0) = 1
    src(1, 0) = 2
    src(2, 0) = 3
    src(3, 0) = 4
    src(0, 1) = 5
    src(1, 1) = 6
    src(2, 1) = 7
    src(3, 1) = 8
    src(0, 2) = 9
    src(1, 2) = 10
    src(2, 2) = 11
    src(3, 2) = 12

    HorizontalBoxBlur.blur(src, dst, 0, 3, 2)

    def check(x: Int, y: Int, expected: Int) =
      assert(dst(x, y) == expected,
        s"(destination($x, $y) should be $expected)")

    check(0, 0, 6)
    check(1, 0, 6)
    check(2, 0, 6)
    check(3, 0, 7)
    check(0, 1, 6)
    check(1, 1, 6)
    check(2, 1, 6)
    check(3, 1, 7)
    check(0, 2, 6)
    check(1, 2, 6)
    check(2, 2, 6)
    check(3, 2, 7)
  }

  def check(width: Int, height: Int, numTasks: Int, radius: Int)
           (blurFunc: (Img,Img,Int,Int) => Unit ) = {
    val src = createRandomImage(width, height)
    val dst = new Img(width, height)
    val check = new Img(width, height)

    def checkFunc(x: Int, y: Int, expected: Int) =
      assert(dst(x, y) == expected,
        s"(destination($x, $y) should be $expected)")

    (0 until src.height).foreach(row =>
      (0 until src.width).foreach(col => check.update(col, row, boxBlurKernel(src, col, row, radius)))
    )

    blurFunc(src, dst, numTasks, radius)

    (0 until src.height).foreach(row =>
      (0 until src.width).foreach { col =>
        checkFunc(col, row, check(col, row))
      }
    )
  }



  def createRandomImage(width: Int, height: Int): Img =
    randomizeImage(new Img(width, height))

  def randomizeImage(src: Img): Img = {
    (0 until src.height).foreach(row =>
      (0 until src.width).foreach(col => src.update(col, row, Random.nextInt()))
    )
    src
  }

  test("HorizontalBoxBlur.parBlur with 32 tasks should execute 32 parallel tasks for a 64x32 image, each blurring one strip") {
    check(10, 10, 2, 1)(HorizontalBoxBlur.parBlur)
    check(10, 10, 10, 2)(HorizontalBoxBlur.parBlur)
    check(10, 5, 5, 2)(HorizontalBoxBlur.parBlur)
    check(64, 32, 32, 1)(HorizontalBoxBlur.parBlur)
    check(32, 64, 32, 1)(HorizontalBoxBlur.parBlur)
  }

  test("VerticalBoxBlur.parBlur with 32 tasks should execute 32 parallel tasks for a 64x32 image, each blurring one strip") {
    check(10, 10, 2, 1)(VerticalBoxBlur.parBlur)
    check(10, 10, 10, 2)(VerticalBoxBlur.parBlur)
    check(10, 5, 5, 2)(HorizontalBoxBlur.parBlur)
    check(64, 32, 32, 1)(HorizontalBoxBlur.parBlur)
  }

  test("HorizontalBoxBlur.parBlur with radius 1 and 4 tasks should correctly blur the entire 3x3 image") {
    check(3,3,4,1)(HorizontalBoxBlur.parBlur)
  }

  test("VerticalBoxBlur.parBlur with radius 1 and 32 tasks should correctly blur the entire 3x3 image") {
    check(3,3,32,1)(VerticalBoxBlur.parBlur)
  }
}
