package IFTProject

import scala.collection.immutable.ListMap
import scala.util.Random

object BatcherBanyan {

  def firstCol(input: String, dest: String): String = {   //Calculating path1 using Banyan Network logic
    var path2 = ""
    if (input == "A1" || input == "A3") {
      if (dest.charAt(0) == '0') {
        path2 = "B1"
      } else path2 = "B3"
    }
    if (input == "A2" || input == "A4") {
      if (dest.charAt(0) == '0')
        path2 = "B2"
      else path2 = "B4"
    }
    path2
  }

  def middleCol(input: String, dest: String): String = {    //Calculating path2 using Banyan Network logic
    var path3 = ""
    if (input == "B1" || input == "B2") {
      if (dest.charAt(1) == '0')
        path3 = "C1"
      else path3 = "C2"
    }
    if (input == "B3" || input == "B4") {
      if (dest.charAt(1) == '0')
        path3 = "C3"
      else path3 = "C4"
    }
    path3
  }

  def lastCol(input: String, dest: String): String = {    //Calculating path4 using Banyan Network logic
    var path4 = ""
    if (input == "C1") {
      if (dest.charAt(2) == '0')
        path4 = "0"
      else path4 = "1"
    }
    if (input == "C2") {
      if (dest.charAt(2) == '0')
        path4 = "2"
      else path4 = "3"
    }
    if (input == "C3") {
      if (dest.charAt(2) == '0')
        path4 = "4"
      else path4 = "5"
    }
    if (input == "C4") {
      if (dest.charAt(2) == '0')
        path4 = "6"
      else path4 = "7"
    }
    path4
  }

  def toBinary(v: Int): String = v.toBinaryString

  def main(args: Array[String]): Unit = {
    var numberVal: List[Int] = List()
    println("Enter the number random values to be generated: ")   //Taking input values for random value generator
    val n = scala.io.StdIn.readInt()
    for (_ <- 0 until n) {    //Generating random values while checking for duplicates
      var rand = Random.between(0, 7)
      while (numberVal.contains(rand))
        rand = Random.between(0, 7)
      numberVal = numberVal :+ rand
      }
    println("\nThe random numbers generated are: "+ numberVal)    //Displaying the random values
    numberVal = numberVal.sorted    //Sorting the random values generated using the principle of Batcher Sorter
    val dict = numberVal.zipWithIndex.map{ case (v,i) => (i,v) }.toMap
    val dict1 = ListMap(dict.toSeq.sortBy(_._2):_*)
    println("\nResult of Batcher Sorting: " +dict1+ "\n")   //Displaying the result of Batcher Sorting
    val start_nodes = Map(0 -> "A1", 1 -> "A2", 2 -> "A3", 3 -> "A4", 4 -> "A1", 5 -> "A2", 6 -> "A3", 7 -> "A4")   //Generating a map of start nodes
    for (i <- 0 until n) {    //Initializing Banyan Network
      val bin = toBinary(dict1(i)).reverse.padTo(3,"0").reverse.mkString    //Converting the values to binary
      val path1 = start_nodes(i)    //Input port
      val path2 = firstCol(path1, bin)    //Calculating path2
      val path3 = middleCol(path2, bin)   //Calculating path3
      val path4 = lastCol(path3, bin)   //Calculating path4
     println("Path for Port: " +i+ " to Output " +dict1(i)+ " is " +path1+ " --> " +path2+ " --> " +path3+ " --> " +path4)    //Displaying the output routes for each value
    }
  }
}
