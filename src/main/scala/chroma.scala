package chroma

private [chroma] object Implicits {
   /** break a string into partions of n size */
   implicit def s2p2(str: String) = new {
    def partionsOf(n: Int) = {
      def par[T](l:List[T])(mkt: List[T] => T): List[T] = l match {
        case l @ Nil => l
        case xs if(xs.size == n) => mkt(xs):: Nil
        case xs => mkt(xs.take(n)) :: par(xs.drop(n))(mkt)
      }
      par(str.toList.map(_.toString))(_.mkString(""))
    }
  }
}

case class RGB(r: Int, g: Int, b: Int) {
  val range = 0 to 255
  r :: g :: b :: Nil map(d => require((0 to 255).contains(d), "%s out of range" format d))

  def hex = Hex(r :: g :: b :: Nil map(Integer.toHexString(_)) mkString(""))
}

case class Hex(hex: String) {
   val range = (0 to 9) ++ ('a' to 'f')
   require((3 :: 6 :: Nil).contains(hex.size), "invalid length")
   hex.toList.foreach(l => require(range.contains(l.toLowerCase), "%s out of range" format l))

   import Implicits._
   def rgb = hex.size match {
     case 3 => hex.partionsOf(1).map(_*2).map(Integer.parseInt(_, 16)) match {
       case List(r, g, b) => RGB(r, g, b)
     }
     case 6 => hex.partionsOf(2).map(Integer.parseInt(_, 16)) match {
       case List(r, g, b) => RGB(r, g, b)
     }
   }

   def cmyk = rgb match {
     case RGB(r, g, b) =>
       if((r :: g :: b :: Nil).forall(_ == 0)) CMYK(0,0,0,1)
       else {
         (r :: g :: b :: Nil).map(v => 1 - v/255) match {
           case c :: m :: y :: Nil =>
             val min = Math.min(c, Math.min(m, y))
             (c  :: m :: y :: Nil).map(v => v - min / (1-min)) match {
               case c1 :: m1 :: y1 :: Nil => CMYK(c1, m1, y1, min)
             }
         }
       }
   }
}


case class CMYK(c: Int, m: Int, y: Int, k: Int) {

}
