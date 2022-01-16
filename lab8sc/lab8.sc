import scala.collection.immutable.Nil
// Jakub Bednarek

//Zadanie 2

sealed trait Graphs[A]
  case class Graph[A](succ: A => List[A]) extends Graphs[A]

sealed trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

val g = Graph((i: Int) =>
  i match
    case 0 => List(1, 2, 3, 4)
    case 1 => List(5, 6)
    case 2 => List(7)
    case 3 => List(8, 9, 10)
    case 4 => List(11, 12, 13, 14)
    case 5 => List(15, 16, 17)
    case 6 => List(18)
    case 7 => Nil
    case 8 => Nil
    case 9 => Nil
    case 10 => Nil
    case 11 => Nil
    case 12 => Nil
    case 13 => Nil
    case 14 => Nil
    case 15 => Nil
    case 16 => Nil
    case 17 => Nil
    case 18 => List(6)
    case n => throw new Exception(s"Graph g: node $n doesn't exist")
)

def add_lists[A](g: Graph[A], elems: List[A]): List[A] =
  def add_lists_rec(currentElements: List[A], accum: List[A]): List[A] =
    currentElements match
      case Nil => accum
      case hd::tl => add_lists_rec(tl, accum ::: (g succ hd))
  add_lists_rec(elems, List())

def get_unvisited_values[A](visited: List[A], values: List[A]): List[A] =
  values match
    case Nil => Nil
    case hd::tl => if !visited.contains(hd) then hd :: get_unvisited_values(visited, tl) else get_unvisited_values(visited, tl)

def graph_to_bt[A](graph1: Graph[A], startNode: A): BT[List[A]] =
  def map_rec(values: List[A], visited: List[A], elems: List[A]): BT[List[A]] =
    var newElems = get_unvisited_values(visited, elems)
    newElems match
      case Nil => if values == Nil then Empty else Node(values, Empty, Empty)
      case hd::tl => Node(values,
                          map_rec(newElems.take(newElems.size / 2), visited ::: values, add_lists(graph1, newElems.take(newElems.size / 2))),
                          map_rec(newElems.drop(newElems.size / 2), visited ::: values, add_lists(graph1, newElems.drop(newElems.size / 2))))
  map_rec(List(startNode), List(startNode), graph1 succ startNode)


graph_to_bt(g, 0)