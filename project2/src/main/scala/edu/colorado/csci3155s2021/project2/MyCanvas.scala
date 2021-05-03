package edu.colorado.csci3155s2021.project2

/* A class to maintain a canvas */
import java.awt.geom.{Ellipse2D, Rectangle2D}
import java.awt.Graphics2D
import scala.::

/* A figure is a sealed trait. It can be a Polygon or a "MyCircle"*/
sealed trait Figure {
    def getBoundingBox: (Double, Double, Double, Double)
    def translate(x: Double, y: Double): Figure
    def render(g: Graphics2D, scaleX: Double, scaleY: Double, shiftX: Double, shiftY: Double): Unit
    // define rotate
    def rotate(theta: (Double,Double,Double,Double)): Figure
}

/*
 Class Polygon
   A polygon is defined by a list of its vertices
 */
case class Polygon(val cList: List[(Double, Double)]) extends Figure {
    //TODO: Define the bounding box of the polygon
    // This function returns a 4-tuple (xmin, xmax, ymin, ymax)
    override def getBoundingBox: (Double, Double, Double, Double ) = {

        val x_min = cList.minBy(_._1)._1
        val x_max = cList.maxBy(_._1)._1
        val y_min = cList.minBy(_._2)._2
        val y_max = cList.maxBy(_._2)._2

        (x_min, x_max, y_min, y_max)
    }


    //TODO: Create a new polygon by shifting each vertex in cList by (x,y)
    //    Do not change the order in which the vertices appear
    override def translate(x: Double, y: Double): Polygon = {
        val p = cList.map(c => (c._1 + x, c._2 + y))
        Polygon(p)
    }


    override def rotate(theta: (Double,Double,Double,Double)): Polygon = {

        val newList = cList.map { case (x, y) => (theta._1 * x + theta._2 * y, theta._3 * x + theta._4 * y)}

        new Polygon(newList)
    }


    // Function: render -- draw the polygon. Do not edit this function.
    override def render(g: Graphics2D, scaleX: Double, scaleY: Double, shiftX: Double, shiftY: Double): Unit = {
        val xPoints: Array[Int] = new Array[Int](cList.length)
        val yPoints: Array[Int] = new Array[Int](cList.length)
        for (i <- 0 until cList.length){
            xPoints(i) = ((cList(i)._1 + shiftX )* scaleX).toInt
            yPoints(i) = ((cList(i)._2 + shiftY) * scaleY).toInt
        }
        g.drawPolygon(xPoints, yPoints, cList.length)

    }
}

/*
  Class MyCircle
  Define a circle with a given center c and radius r
 */
case class MyCircle(val c: (Double, Double), val r: Double) extends Figure {
    //TODO: Define the bounding box for the circle
    override def getBoundingBox: (Double, Double, Double, Double) = {
        val x_min = (c._1) - r
        val x_max = (c._1) + r
        val y_min = (c._2) - r
        val y_max = (c._2) + r

        (x_min, x_max, y_min, y_max)
    }


    //TODO: Create a new circle by shifting the center
    override def translate(x: Double, y: Double): MyCircle = {
        val cir = (c._1 + x, c._2 + y)
        MyCircle(cir, r)
    }


    override def rotate(theta: (Double,Double,Double,Double)): MyCircle = {

        val NewCenter = (theta._1 * c._1 + theta._2 * c._2, theta._3 * c._1 + theta._4 * c._2)

        new MyCircle(NewCenter, r)

    }


    // Function: render -- draw the circle. Do not edit this function
    override def render(g: Graphics2D, scaleX: Double, scaleY: Double, shiftX: Double, shiftY: Double) = {
        val centerX = ((c._1 + shiftX) * scaleX) .toInt
        val centerY = ((c._2 + shiftY) * scaleY) .toInt
        val radX = (r * scaleX).toInt
        val radY = (r * math.abs(scaleY)).toInt
        //g.draw(new Ellipse2D.Double(centerX, centerY, radX, radY))
        g.drawOval(centerX-radX, centerY-radY, 2*radX, 2*radY)
    }
}

/*
  Class : MyCanvas
  Define a canvas through a list of figure objects. Figure objects can be circles or polygons.
 */

class MyCanvas (val listOfObjects: List[Figure]) {
    // TODO: Write a function to get the boundingbox for the entire canvas.
    // Hint: use existing boundingbox functions defined in each figure.
    def getBoundingBox: (Double, Double, Double, Double) = {
        val x_min = listOfObjects.map(x => x.getBoundingBox).minBy(_._1)._1
        val x_max = listOfObjects.map(x => x.getBoundingBox).maxBy(_._2)._2
        val y_min = listOfObjects.map(x => x.getBoundingBox).minBy(_._3)._3
        val y_max = listOfObjects.map(x => x.getBoundingBox).maxBy(_._4)._4

        (x_min, x_max, y_min, y_max)
    }

    //TODO: Write a function to translate each figure in the canvas by shiftX, shiftY
    def translate(shiftX: Double, shiftY: Double): MyCanvas = {
        val f: List[Figure] = listOfObjects.map(x => x.translate(shiftX, shiftY))
        new MyCanvas(f)
    }


    //TODO: Write a function that will return a new MyCanvas object that places
    // all the objects in myc2 to the right of the objects in this MyCanvas.
    // refer to the notebook documentation on how to perform this.
    def placeRight(myc2: MyCanvas):MyCanvas = {
        val (x_min1, x_max1, y_min1, y_max1) = this.getBoundingBox
        val (x_min2, x_max2, y_min2, y_max2) = myc2.getBoundingBox
        val shiftX = (x_max1 - x_min2)
        val shiftY = (y_max1 - y_min1)/2 - (y_max2 - y_min2)/2
        val c2 = myc2.translate(shiftX, shiftY)
        overlap(c2)
    }


    //TODO: Write a function that will return a new MyCanvas object that places
    // all the figures in myc2 on top of the figures in this MyCanvas.
    // refer to the notebook documentation on how to perform this.
    def placeTop(myc2: MyCanvas): MyCanvas = {
        // this = c1
        val (x_min1, x_max1, y_min1, y_max1) = this.getBoundingBox
        val (x_min2, x_max2, y_min2, y_max2) = myc2.getBoundingBox
        val shiftX = (x_max1 - x_min1)/2 - (x_max2 - x_min2)/2
        val shiftY = (y_max1 - y_min2)
        val c2 = myc2.translate(shiftX, shiftY)
        overlap(c2)
    }


    //TODO: Write a function that will rotate each figure about the center of its bounding box in the canvas using
    // the angle `ang` defined in radians.
    // The writeup provided describes how to implement the rotation.
    // Hint: Write helper functions to rotate a Polygon and a circle. Then you can simply use
    // translation, followed by rotation of individual objects and translation back.
    def rotate(angRad: Double): MyCanvas = {

        val (x_min, x_max, y_min, y_max) = this.getBoundingBox

        val x_c = (x_max + x_min)/2
        val y_c = (y_max + y_min)/2

        val myc_t = listOfObjects.map(x => x.translate((-x_c), (-y_c)))


        val x_p1 = math.cos(angRad)
        val x_p2 = -math.sin(angRad)
        val y_p1 = math.sin(angRad)
        val y_p2 = math.cos(angRad)

        val newList = myc_t.map(x => x.rotate((x_p1, x_p2, y_p1, y_p2))) // all 4 args here


        // map again
        val myc_final = newList.map(x => x.translate(x_c, y_c))

        new MyCanvas(myc_final)

    }

    def overlap(c2: MyCanvas): MyCanvas = {
        new MyCanvas(listOfObjects ++ c2.listOfObjects)
    }

    // Function to draw the canvas. Do not edit.
    def render(g: Graphics2D, xMax: Double, yMax: Double) = {
        val (lx1, ux1, ly1, uy1) = this.getBoundingBox
        val shiftx = -lx1
        val shifty = -uy1
        val scaleX = xMax/(ux1 - lx1  + 1.0)
        val scaleY = yMax/(uy1 - ly1 + 1.0)
        listOfObjects.foreach(f => f.render(g,scaleX, -scaleY, shiftx, shifty))
    }

    // DO NOT EDIT THE CODE BELOW
    override def toString: String = {
        listOfObjects.foldLeft[String] ("") { case (acc, fig) => acc ++ fig.toString }
    }

    // DO NOT EDIT
    def getListOfObjects: List[Figure] = listOfObjects

    // DO NOT EDIT
    def numPolygons: Int =
        listOfObjects.count {
            case Polygon(_) => true
            case _ => false }

    // DO NOT EDIT
    def numCircles: Int = {
        listOfObjects.count {
            case MyCircle(_,_) => true
            case _ => false }
    }
    // DO NOT EDIT
    def numVerticesTotal: Int = {
        listOfObjects.foldLeft[Int](0) ((acc, f) =>
            f match {
                case Polygon(lst1) => acc + lst1.length
                case _ => acc
            }
        )
    }
}
