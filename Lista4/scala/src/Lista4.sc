// Jakub Bednarek

// Zadanie 3

sealed trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

val testTree = Node(1, Node(2, Node(4, Empty, Empty), Empty), Node(3, Node(5, Empty, Node(6, Empty, Empty)), Empty))
val testTree2 = Node(7, Node(10, Empty, Empty), Node(13, Node(5, Empty, Node(9, Empty, Empty)), Node(4, Empty, Node(2, Node(6, Empty, Empty), Node(3, Empty, Empty)))))
val testTree3 = Empty

def breadthBT[A](bt: BT[A]): List[A] = {
  def bfs[A](nodes: List[BT[A]], labels: List[A]): List[A] = {
    if nodes == Nil then labels
    else
      nodes.head match {
      case Empty => bfs(nodes.tail, labels)
      case Node(v, l, r) => bfs(nodes.tail ++ List(l, r), v :: labels)
    }
  }

  bfs(List(bt), List()).reverse
}

breadthBT(testTree) == List(1, 2, 3, 4, 5, 6)
breadthBT(testTree2) == List(7, 10, 13, 5, 4, 9, 2, 6, 3)
breadthBT(testTree3) == Nil

// Zadanie 4
def outerLength[A](bt: BT[A]): Int =
  def outerLengthR(node: BT[A], depth: Int): Int =
    node match
      case Empty => depth
      case Node(v, l, r) => outerLengthR(l, depth + 1) + outerLengthR(r, depth + 1)
  outerLengthR(bt, 0)

outerLength(testTree) == 21
outerLength(testTree2) == 38
outerLength(testTree3) == 0

def innerLength[A](bt: BT[A]): Int =
  def innerLengthR(node: BT[A], depth: Int): Int =
    node match
      case Empty => 0
      case Node(v, l, r) => depth + innerLengthR(l, depth + 1) + innerLengthR(r, depth + 1)
  innerLengthR(bt, 0)

innerLength(testTree) == 9
innerLength(testTree2) == 20
innerLength(testTree3) == 0

// Zadanie 5
sealed trait Graphs[A]
  case class Graph[A](succ: A => List[A]) extends Graphs[A]

val g = Graph((i: Int) =>
  i match
    case 0 => List(3)
    case 1 => List(0, 2, 4)
    case 2 => List(1)
    case 3 => Nil
    case 4 => List(0, 2)
    case n => throw new Exception(s"Graph g: node $n doesn't exist")
)

val graphTest1 = Graph((i: Int) =>
  i match
    case 2 => List(6)
    case 3 => List(5)
    case 4 => List(8, 2)
    case 5 => List(4)
    case 6 => List(7, 10)
    case 7 => List()
    case 8 => List()
    case 10 => List()
    case 12 => List(3, 8)
    case n => throw new Exception(s"Graph graphTest1: node $n doesn't exist")
)

val graphTest2 = Graph((i: Int) =>
  i match
    case n => throw new Exception(s"Graph graphTest2: node $n doesn't exist")
)

def depthSearch[A](graph: Graph[A])(startNode: A): List[A] =
  def dfs(visited: List[A])(queue: List[A]): List[A] =
    queue match
      case Nil => Nil
      case h :: t =>
        if visited contains h then dfs(visited)(t)
        else h :: dfs(h :: visited)((graph succ h) ::: t)
  dfs(Nil)(List(startNode))

depthSearch(g)(4) == List(4, 0, 3, 2, 1)
depthSearch(graphTest1)(12) == List(12, 3, 5, 4, 8, 2, 6, 7, 10)
//depthSearch(graphTest2)(3)