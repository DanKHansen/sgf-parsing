import scala.util.parsing.combinator.*

object Sgf extends RegexParsers:

   private type Tree[A] = Node[A] // to separate the type from the constructor, cf. Haskell's Data.Tree
   private type Forest[A] = List[Tree[A]]
   case class Node[A](rootLabel: A, subForest: Forest[A] = Nil)

   // A tree of nodes.
   private type SgfTree = Tree[SgfNode]

   // A node is a property list, each key can only occur once.
   // Keys may have multiple values associated with them.
   private type SgfNode = Map[String, List[String]]

   private def gameTree: Parser[SgfTree] = ???


   def parseSgf(text: String): Option[SgfTree] = {
      parseAll(gameTree, text) match
         case Success(result, _) => Some(result)
         case Failure(msg, next) =>
            println(s"Parsing failed: $msg at line ${next.pos.line}, column ${next.pos.column}"); None
         case Error(msg, next)   =>
            println(s"Parsing Error: $msg at line ${next.pos.line}, column ${next.pos.column}"); None
   }
