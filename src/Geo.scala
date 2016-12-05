/**
  * Created by vatsal on 5/12/16.
  */
class Geo {

  def coordToVector(A: Array[Int], B: Array[Int]): Array[Int] ={
    var result = new Array[Int](2)
    result(0) = B(0)-A(0)
    result(1) = B(1)-A(1)
    return result
  }

  def dot(A: Array[Int], B: Array[Int], C: Array[Int]): Int = {
    //Compute the dot product A.B
    val AB = coordToVector(A, B)
    val BC = coordToVector(B, C)
    val dotP = (AB(0) * BC(0)) + (AB(1) * BC(1))
    return dotP
  }

  def cross(A: Array[Int], B: Array[Int], C: Array[Int]): Int = {
    //Compute the cross product AxB
    val AB = coordToVector(A, B)
    val AC = coordToVector(A, C)
    val crossP = (AB(0) * AC(1)) - (AB(1) * AC(0))
    return crossP
  }

  def distance(A: Array[Int], B: Array[Int]): Double = {
    //Compute the distance from A to B
    val d1 = A(0) - B(0)
    val d2 = A(1) - B(1)
    val result = Math.sqrt(d1*d1 + d2*d2)
    return result
  }

  def linePointDistance(A: Array[Int], B: Array[Int], C: Array[Int], isSegment: Boolean): Double = {
    //Compute the distance from AB to C
    //if isSegment is true, AB is a segment, not a line
    val dist = cross(A, B, C) / distance(A, B)
    if(isSegment){
      val dot1 = dot(A, B, C)
      if(dot1 > 0) return distance(B,C)
      val dot2 = dot(B, A, C)
      if(dot2 > 0) return distance(A,C)
    }
    return Math.abs(dist)
  }

  def areaOfPolygon(polygon: Array[Array[Int]]): Double = {
    /*
      Calculates the area of a polygon with its vertices passed in a 2D array
      We will triangulate the polygon into triangles with points p[0],p[i],p[i+1]
    */
    var area = 0
    val len = polygon.length
    for(i <- 1 to len-2){
      val x1 = polygon(i)(0) - polygon(0)(0)
      val y1 = polygon(i)(1) - polygon(0)(1)
      val x2 = polygon(i+1)(0) - polygon(0)(0)
      val y2 = polygon(i+1)(1) - polygon(0)(1)
      val cross = x1*y2 - x2*y1
      area += cross
    }
    val result = Math.abs(area/2.0)
    return result
  }

}
