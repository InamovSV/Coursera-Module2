case class Pos(val x: Int, val y: Int){

}

class Block(b1: Pos, b2: Pos){
  def isStanding = b1 == b2
}

val b = new Block(Pos(1,1),Pos(1,1))
b.isStanding
val p1 = new Pos(1,1)
val p2 = new Pos(1,1)
p1 == p2