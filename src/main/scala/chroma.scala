package chroma

private [chroma] object Implicits {
   /** break a string into partions of n size */
   implicit def s2p2(str: String) = new {
    def partionsOf(n: Int) = {
      def par[T](l: List[T])(mkt: List[T] => T): List[T] = l match {
        case l @ Nil => l
        case xs if(xs.size == n) => mkt(xs) :: Nil
        case xs => mkt(xs.take(n)) :: par(xs.drop(n))(mkt)
      }
      par(str.toList.map(_.toString))(_.mkString(""))
    }
  }
}

trait Color

case class Rgb(r: Int, g: Int, b: Int) extends Color {
  val range = 0 to 255
  r :: g :: b :: Nil map(d => require((0 to 255).contains(d), "%s must be between 0..255" format d))

  def hex = Hex(r :: g :: b :: Nil map(Integer.toHexString(_)) map(s => if(s.size==1) s * 2 else s) mkString(""))

  def cmyk = {
    if((r :: g :: b :: Nil).forall(_ == 0)) CMYK(0, 0, 0, 1)
    else {
      (r :: g :: b :: Nil).map(v => 1 - (v.toDouble / 255)) match {
        case c :: m :: y :: Nil =>
          val black = Math.min(c, Math.min(m, y))
          (c  :: m :: y :: Nil).map(v => v - black / (1 - black)) match {
            case c1 :: m1 :: y1 :: Nil => CMYK(c1, m1, y1, black)
          }
      }
    }
  }

  def hsv = {
    (r :: g :: b :: Nil).map(_.toDouble / 255) match {
      case r2 :: g2 :: b2 :: Nil =>
        val (min, max) = (Math.min(r2, Math.min(g2, b2)), Math.max(r2, Math.max(g2, b2)))
        val delta = max - min
        if(min == max) {
          Hsv(0, 0, min) // monochrome
        } else {
          Hsv((max match {
             case n if(n == r2) => (g2 - b2) / delta + (if(g2 < b2) 6 else 0)
             case n if(n == g2) => (b2 - r2) / delta + 2
             case n => (r2 - g2) / delta + 4
          }) * 60, if(max == 0)  0 else delta / max , max)
        }
    }
  }
}

case class Hsv(h: Double, s: Double, v: Double) extends Color

case class Hex(hex: String) extends Color {
   val range = ((0 to 9) ++ ('a' to 'f')).map(_.toString.charAt(0))
   require((3 :: 6 :: Nil).contains(hex.size), "invalid length")
   hex.toList.foreach(l => require(range.contains(l.toLowerCase), "%s must be one of %s" format(l, range)))

   import Implicits._
   def rgb = hex.size match {
     case 3 => hex.partionsOf(1).map(_*2).map(Integer.parseInt(_, 16)) match {
       case List(r, g, b) => Rgb(r, g, b)
     }
     case 6 => hex.partionsOf(2).map(Integer.parseInt(_, 16)) match {
       case List(r, g, b) => Rgb(r, g, b)
     }
   }

   def cmyk = rgb.cmyk

   def hsv = rgb.hsv

}


case class CMYK(c: Double, m: Double, y: Double, k: Double) extends Color
