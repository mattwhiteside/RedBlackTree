//import scala.collection.immutable.{StringLike, StringOps}

/**
 * Created by matt on 7/10/13.
 */


object RBTreeColors extends Enumeration {
  type RBColor = Value
  val Red, Black = Value
}



class RedBlackTree[K <% Ordered[K], V] {

  import RBTreeColors._

  class Node(val key: K, var value: V, var color: RBColor = Black){



    var left: Option[Node] = None
    var right: Option[Node] = None
    var parent: Option[Node] = None

    def grandparent : Option[Node] = {
      val p: Option[Node] = parent
      p match {
        case Some(p) => p.parent
        case None => None
      }
    }

    def sibling : Option[Node] = {
      val p = parent
      p match{
        case Some(p) => if(this == p.left){
          p.right
        } else{
          p.left
        }
        case None => None
      }
    }


    def uncle : Option[Node] = {
      val gp = grandparent
      gp match{
        case Some(gp) => if (parent == gp.left){
          gp.right
        } else{
          gp.left
        }
        case None => None
      }
    }
  }


  def insert(key: K, value: V) : Unit = {
    val newNode = new Node(key, value, Red);
    root match{

      case Some(root) => {
        var currentNode = root
        while (newNode.parent == None){
          if (newNode.key < currentNode.key){
            val leftChild = currentNode.left
            leftChild match {
              case Some(leftChild) => currentNode = leftChild
              case None => {
                currentNode.left = Option(newNode)
                newNode.parent = Option(currentNode)
              }
            }
          } else{
            val rightChild = currentNode.right
            rightChild match {
              case Some(rightChild) => currentNode = rightChild
              case None => {
                currentNode.right = Option(newNode)
                newNode.parent = Option(currentNode)
              }
            }
          }
        }
      }
      case None => root = Option(newNode)
    }
    rebalanceFrom(newNode)

  }

  //case 1
  def rebalanceFrom(node: Node) : Unit = {
    node.parent match{
      case None => node.color = Black//just inserted the first (aka root) node into the tree
      case Some(node) => {
        rebalanceFromBelowRoot(node)
      }
    }
  }

  //case 2
  def rebalanceFromBelowRoot(node: Node) {
    colorOf(node.parent) match {
      case Black => {/*do nothing; tree is still valid*/}
      case Red => rebalanceBelowRedParent(node)
    }
  }

  //case 3
  def rebalanceBelowRedParent(child: Node){
    colorOf(child.uncle) match {
      case Red => {
        val p = child.parent
        p match {
          case Some(p) => p.color = Black
          case None => {/*do nothing*/}
        }
        val u = child.uncle
        u match {
          case Some(u) => u.color = Red
          case None => {/*nothing*/}
        }
        val g = child.grandparent
        g match {
          case Some(g) => {
            g.color = Red
            rebalanceFrom(g)
          }
          case None => {/*nothing*/}
        }
      }
      case Black => rebalanceSwitchbackGrandchild(child)
    }
  }

  //case 4: node is the right child and its parent is the left child of node's grandparent
  //     or node is the left  child and its parent is the right child of node's grandparent
  def rebalanceSwitchbackGrandchild(grandchild: Node){
    val parent = grandchild.parent

    parent match {
      case None => {/*nothing: we can assume that if this is getting called, the parent is present*/}
      case Some(parent) => {
        val grandparent = parent.parent
        var otherGrandchild = Option(grandchild)
        grandparent match {
          case None => {/*nothing*/}
          case Some(grandparent) => {
            if((grandchild eq parent.right) && (parent eq grandparent.left)){
              rotateLeft(parent)
              otherGrandchild = grandchild.left
            } else if ((grandchild eq parent.left) && (parent eq grandparent.right)){
              rotateRight(parent)
              otherGrandchild = grandchild.right
            }
            otherGrandchild match {
              case Some(otherGrandchild) => rebalanceInlineGrandchild(otherGrandchild)
              case None => {/*nothing*/}
            }

          }
        }

      }
    }
  }

  //case 5
  def rebalanceInlineGrandchild(grandchild: Node){
    val parent = grandchild.parent
    parent match {
      case None => {}
      case Some(parent) => {
        parent.color = Black
        val grandparent = parent.parent
        grandparent match {
          case None => {}
          case Some(grandparent) => {
            grandparent.color = Red
            if ((grandchild eq parent.left) && (parent eq grandparent.left)){
              rotateRight(grandparent)
            } else{
              rotateLeft(grandparent)
            }
          }
        }
      }
    }
  }

  def rotateLeft(node: Node){
    val right = node.right
    replace(node, right)

    node.right = right match {
      case None => None
      case Some(right) => {
        val left =  right.left
        left match {
          case None => {}
          case Some(left) => left.parent = Option(node)
        }
        right.left = Option(node)
        left
      }
    }

    node.parent = right
  }

  def rotateRight(node: Node){
    val left = node.left
    replace(node, left)
    node.left = left match{
      case None => None
      case Some(left) => {
        val right = left.right
        right match{
          case None => {}
          case Some(right) => right.parent = Option(node)
        }
        left.right = Option(node)
        right
      }
    }
    node.parent = left
  }

  def replace(oldNode: Node, newNode: Option[Node]){
    val oldParent = oldNode.parent
    oldParent match {
      case None => root = newNode
      case Some(oldParent) => {
        if (oldNode eq oldParent.left){
          oldParent.left = newNode
        } else{
          oldParent.right = newNode
        }
      }
    }
    newNode match {
      case None => {}
      case Some(newNode) => {
        newNode.parent = oldParent
      }
    }
  }

  def colorOf(n: Option[Node]) : RBColor = {
    n match {
      case Some(n) => n.color
      case None => Black
    }
  }


  var root: Option[Node] = None
  var size = 0




}

object RedBlackTreeTest extends App {
  val tree = new RedBlackTree[String, Array[String]]
  var counter = 0
  scala.io.Source.fromFile("/Users/matt/Desktop/query_result.csv").getLines().drop(1).map(_.split(",")).slice(0,9).foreach {   row =>
    val email = row(1)
    val zip = row(3)
    val key = s"$email-$zip"
    println(key)
    tree.insert(key, row)
    counter += 1
  }
}