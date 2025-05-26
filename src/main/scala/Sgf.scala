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

   // Parser for an identifier (property key): one or more uppercase letters
   private def ident: Parser[String] = """[A-Z]+""".r

   // Parser for a property (e.g., `B[aa]`, `W[bb]`)
   private def property: Parser[(String, List[String])] =
      ident ~ "[" ~ repsep(ident, ",") ~ "]" ^^ { case key ~ _ ~ values ~ _ =>
         (key, values)
      }

   // Parser for a sequence of properties starting with ';'
   private def node: Parser[SgfNode] =
      ";" ~> rep(property) ^^ { props =>
         // Each key can only occur once; combine all values into a list
         props.groupMapReduce(_._1)(_._2)(_ ++ _)
      }

   private def gameTree: Parser[SgfTree] =
      "(" ~> rep1(node) <~ ")" ^^ { nodeSeq =>
         def chainNodes(nodes: Seq[SgfNode]): SgfTree = nodes match
            case Seq() => throw new RuntimeException("Unexpected empty node sequence")
            case Seq(single) => Node(single)
            case Seq(head, tail*) => Node(head, List(chainNodes(tail)))
         chainNodes(nodeSeq)
      }


   def parseSgf(text: String): Option[SgfTree] =
      parseAll(gameTree, text) match
         case Success(result, _) => Some(result)
         case Failure(msg, next) =>
            println(s"Parsing failed: $msg at line ${next.pos.line}, column ${next.pos.column}")
            None
         case Error(msg, next) =>
            println(s"Parsing error: $msg at line ${next.pos.line}, column ${next.pos.column}")
            None
