import org.scalacheck._
import Prop._
import Gen._
import Arbitrary._

import scala.reflect.runtime.universe._
import Flag._

object TermConstructionProps extends QuasiquoteProperties("term construction") {

  property("splice single tree return tree itself") = forAll { (t: Tree) =>
    q"$t" ≈ t
  }

  property("splice trees into if expression") = forAll { (t1: Tree, t2: Tree, t3: Tree) =>
    val Apply(Ident(__ifThenElse), List(cond1, then1, else1)) = q"if($t1) $t2 else $t3"
    __ifThenElse == newTermName("__ifThenElse") && cond1 ≈ t1 && then1 ≈ t2 && else1 ≈ t3
  }

  property("splice trees into ascriptiopn") = forAll { (t1: Tree, t2: Tree) =>
    q"$t1 : $t2" ≈ Typed(t1, t2)
  }

  property("splice trees into apply") = forAll { (t1: Tree, t2: Tree, t3: Tree) =>
    q"$t1($t2, $t3)" ≈ Apply(t1, List(t2, t3))
  }

  property("splice trees with .. cardinality into apply") = forAll { (ts: List[Tree]) =>
    q"f(..$ts)" ≈ Apply(q"f", ts)
  }

  property("splice iterable into apply") = forAll { (trees: List[Tree]) =>
    val itrees: Iterable[Tree] = trees
    q"f(..$itrees)" ≈ Apply(q"f", trees)
  }

  property("splice trees with ... cardinality into apply") = forAll { (ts1: List[Tree], ts2: List[Tree]) =>
    val argss = List(ts1, ts2)
    q"f(...$argss)" ≈ Apply(Apply(q"f", ts1), ts2)
  }

  property("splice term name into assign") = forAll { (name: TermName, t: Tree) =>
    val Apply(Ident(__assign), List(name1, t1)) = q"$name = $t"
    __assign == newTermName("__assign") && name1 ≈ Ident(name) && t1 ≈ t
  }

  property("splice trees into block") = forAll { (t1: Tree, t2: Tree, t3: Tree) =>
    blockInvariant(q"""{
      $t1
      $t2
      $t3
    }""", List(t1, t2, t3))
  }


  property("splice tree into new") = forAll { (tree: Tree) =>
    q"new $tree" ≈ Apply(Select(New(tree), nme.CONSTRUCTOR), List())
  }

  property("splice tree into return") = forAll { (tree: Tree) =>
    val Apply(Ident(__return), List(tree1)) = q"return $tree"
    __return == newTermName("__return") && tree1 ≈ tree
  }

  property("splice a list of arguments") = forAll { (fun: Tree, args: List[Tree]) =>
    q"$fun(..$args)" ≈ Apply(fun, args)
  }

  property("splice list and non-list fun arguments") = forAll { (fun: Tree, arg1: Tree, arg2: Tree, args: List[Tree]) =>
    q"$fun(..$args, $arg1, $arg2)" ≈ Apply(fun, args ++ List(arg1) ++ List(arg2)) &&
    q"$fun($arg1, ..$args, $arg2)" ≈ Apply(fun, List(arg1) ++ args ++ List(arg2)) &&
    q"$fun($arg1, $arg2, ..$args)" ≈ Apply(fun, List(arg1) ++ List(arg2) ++ args)
  }

  property("splice into new") = forAll { (name: TypeName, body: List[Tree]) =>
    q"new $name { ..$body }" ≈
      q"""{
        final class $$anon extends $name {
          ..$body
        }
        new $$anon
      }"""
  }

  property("splice type name into this") = forAll { (T: TypeName) =>
    q"$T.this" ≈ This(T)
  }

  property("splice tree into throw") = forAll { (t: Tree) =>
    q"throw $t" ≈ Throw(t)
  }

  property("splice trees into type apply") = forAll { (fun: TreeIsTerm, types: List[Tree]) =>
    q"$fun[..$types]" ≈ (if (types.nonEmpty) TypeApply(fun, types) else fun)
  }

  property("splice names into import selector") = forAll {
    (expr: Tree, plain: Name, oldname: Name, newname: Name, discard: Name) =>

    val Import(expr1, List(
      ImportSelector(plain11, _, plain12, _),
      ImportSelector(oldname1, _, newname1, _),
      ImportSelector(discard1, _, wildcard, _))) =
        q"import $expr.{$plain, $oldname => $newname, $discard => _}"

    expr1 ≈ expr && plain11 == plain12 && plain12 == plain &&
    oldname1 == oldname && newname1 == newname && discard1 == discard && wildcard == nme.WILDCARD
  }

  property("splice trees into while loop") = forAll { (cond: Tree, body: Tree) =>
    val Apply(Ident(__whileDo), List(cond1, body1)) = q"while($cond) $body"
    __whileDo == newTermName("__whileDo") && body1 ≈ body && cond1 ≈ cond
  }

  property("splice trees into do while loop") = forAll { (cond: Tree, body: Tree) =>
    val Apply(Ident(__doWhile), List(body1, cond1)) = q"do $body while($cond)"
    __doWhile == newTermName("__doWhile") && body1 ≈ body && cond1 ≈ cond
  }

  property("splice trees into alternative") = forAll { (c: Tree, A: Tree, B: Tree) =>
    q"$c match { case $A | $B => }" ≈
      Match(c, List(
        CaseDef(Alternative(List(A, B)), EmptyTree, Literal(Constant(())))))
  }

  def blockInvariant(quote: Tree, trees: List[Tree]) =
    quote ≈ (trees match {
      case Nil => q"()"
      case _ :+ last if !last.isTerm => Block(trees, q"()")
      case head :: Nil => head
      case init :+ last => Block(init, last)
    })

  property("splice list of trees into block (1)") = forAll { (trees: List[Tree]) =>
    blockInvariant(q"{ ..$trees }", trees)
  }

  property("splice list of trees into block (2)") = forAll { (trees1: List[Tree], trees2: List[Tree]) =>
    blockInvariant(q"{ ..$trees1 ; ..$trees2 }", trees1 ++ trees2)
  }

  property("splice list of trees into block (3)") = forAll { (trees: List[Tree], tree: Tree) =>
    blockInvariant(q"{ ..$trees; $tree }", trees :+ tree)
  }

  property("splice term into brackets") = test {
    val a = q"a"
    assert(q"($a)" ≈ a)
  }

  property("splice terms into tuple") = test {
    val a1 = q"a1"
    val a2 = q"a2"
    val as = List(a1, a2)
    assert(q"(..$as)" ≈ q"Tuple2($a1, $a2)")
    assert(q"(a0, ..$as)" ≈ q"Tuple3(a0, $a1, $a2)")
  }

  property("splice empty list into tuple") = test {
    val empty = List[Tree]()
    assert(q"(..$empty)" ≈ q"()")
  }

  property("function param flags are the same") = test {
    val xy = q"val x: A" :: q"val y: B" :: Nil
    assertEqAst(q"(..$xy) => x + y", q"(x: A, y: B) => x + y")
  }

  property("anonymous functions don't support default values") = test {
    val x = q"val x: Int = 1"
    assertThrows[IllegalArgumentException] { q"($x) => x" }
  }
}
