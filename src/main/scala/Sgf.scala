import scala.util.parsing.combinator.*

/*
  "..." : terminal symbols
  [...] : option: occurs at most once
  {...} : repetition: any number of times, including zero
  (...) : grouping
    |   : exclusive or
 italics: parameter explained at some other place

  Collection = GameTree { GameTree }
  GameTree   = "(" Sequence { GameTree } ")"
  Sequence   = Node { Node }
  Node       = ";" { Property }
  Property   = PropIdent PropValue { PropValue }
  PropIdent  = UcLetter { UcLetter }
  PropValue  = "[" CValueType "]"
  CValueType = (ValueType | Compose)
  ValueType  = (Text)

  Text: is a formatted text.
   Newlines are removed if they come immediately after a \, otherwise they remain as newlines.
   All whitespace characters other than newline are converted to spaces.
   \ is the escape character.
   Any non-whitespace character after \ is inserted as-is.
   Any whitespace character after \ follows the above rules.
   Note that SGF does not have escape sequences for whitespace characters such as \t or \n.

 */

object Sgf extends RegexParsers:

   private type Tree[A] = Node[A] // to separate the type from the constructor, cf. Haskell's Data.Tree
   private type Forest[A] = List[Tree[A]]
   case class Node[A](rootLabel: A, subForest: Forest[A] = Nil)

   // A tree of nodes.
   private type SgfTree = Tree[SgfNode]

   // A node is a property list, each key can only occur once.
   // Keys may have multiple values associated with them.
   private type SgfNode = Map[String, List[String]]



   def parseSgf(text: String): Option[SgfTree] = None
