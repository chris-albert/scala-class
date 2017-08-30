
import common._

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    lazy val widthRange  = 0 until width
    lazy val heightRange = 0 until height
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))
    def apply(x: Int, y: Int): RGBA = data(y * width + x)
    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
    def foreach(f: (Int,Int) => Unit): Unit =
      heightRange.foreach(row => widthRange.foreach(col => f(col,row)))
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel2(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    // TODO implement using while loops
    val pixels = validRadiusCoords(x, y, radius, src).map {case (xc,yc) => src(xc, yc)}
    val total = pixels.size
    val (sr,sg,sb,sa) = pixels.foldLeft((0,0,0,0)){case ((sumR,sumG,sumB,sumA),next) =>
      (sumR + red(next),sumG + green(next),sumB + blue(next),sumA + alpha(next))
    }
    rgba(sr / total,sg / total,sb / total,sa / total)
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    // TODO implement using while loops
    var yi = clamp(y - radius,0,y)
    val ym = clamp(y + radius,0,src.height - 1)
    val xt = clamp(x - radius,0,x)
    var xi = xt
    val xm = clamp(x + radius,0,src.width - 1)
    val pixels = scala.collection.mutable.ArrayBuffer.empty[RGBA]
//    println(s"yi: $yi ym: $ym xi: $xi xm: $xm")
    while(yi <= ym) {
      while(xi <= xm) {
        pixels += src(xi,yi)
        xi = xi + 1
      }
      yi = yi + 1
      xi = xt
    }
    val total = pixels.size
    val (sr,sg,sb,sa) = pixels.foldLeft((0,0,0,0)){case ((sumR,sumG,sumB,sumA),next) =>
      (sumR + red(next),sumG + green(next),sumB + blue(next),sumA + alpha(next))
    }
    rgba(sr / total,sg / total,sb / total,sa / total)
  }

  def radiusCoords(x: Int, y: Int,r: Int): Stream[(Int,Int)] = {
    val width = (r * 2) + 1
    (0 until width).toStream.flatMap{yl =>
      (0 until width).toStream.map{xl =>
        (x - r + xl) -> (y - r + yl)
      }
    }
  }

  def validCoord(x: Int,y: Int, img: Img): Boolean =
    x >= 0 && x < img.width && y >= 0 && y < img.height

  def validRadiusCoords(x: Int,y: Int,r: Int, img: Img): Stream[(Int,Int)] =
    radiusCoords(x, y, r).filter{case (xl,yl) => validCoord(xl, yl, img)}


}
