package patmat

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import patmat.Huffman._
import sun.security.util.Length

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)

    val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s', 121895), Fork(Leaf('d', 56269), Fork(Fork(Fork(Leaf('x', 5928), Leaf('j', 8351), List('x', 'j'), 14279), Leaf('f', 16351), List('x', 'j', 'f'), 30630), Fork(Fork(Fork(Fork(Leaf('z', 2093), Fork(Leaf('k', 745), Leaf('w', 1747), List('k', 'w'), 2492), List('z', 'k', 'w'), 4585), Leaf('y', 4725), List('z', 'k', 'w', 'y'), 9310), Leaf('h', 11298), List('z', 'k', 'w', 'y', 'h'), 20608), Leaf('q', 20889), List('z', 'k', 'w', 'y', 'h', 'q'), 41497), List('x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 72127), List('d', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 128396), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 250291), Fork(Fork(Leaf('o', 82762), Leaf('l', 83668), List('o', 'l'), 166430), Fork(Fork(Leaf('m', 45521), Leaf('p', 46335), List('m', 'p'), 91856), Leaf('u', 96785), List('m', 'p', 'u'), 188641), List('o', 'l', 'm', 'p', 'u'), 355071), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u'), 605362), Fork(Fork(Fork(Leaf('r', 100500), Fork(Leaf('c', 50003), Fork(Leaf('v', 24975), Fork(Leaf('g', 13288), Leaf('b', 13822), List('g', 'b'), 27110), List('v', 'g', 'b'), 52085), List('c', 'v', 'g', 'b'), 102088), List('r', 'c', 'v', 'g', 'b'), 202588), Fork(Leaf('n', 108812), Leaf('t', 111103), List('n', 't'), 219915), List('r', 'c', 'v', 'g', 'b', 'n', 't'), 422503), Fork(Leaf('e', 225947), Fork(Leaf('i', 115465), Leaf('a', 117110), List('i', 'a'), 232575), List('e', 'i', 'a'), 458522), List('r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 881025), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u', 'r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 1486387)
    val secret: List[Bit] = List(0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1)
    val test1 = Fork(Leaf('a', 23), Fork(Fork(Leaf('b', 15), Leaf('c', 8), List('b', 'c'), 23), Leaf('d', 5), List('b', 'c', 'd'), 28), List('a', 'b', 'c', 'd'), 51)
    val someText: CodeTree = Fork(Fork(Fork(Fork(Fork(Fork(Leaf('T', 1), Leaf('e', 2), List('T', 'e'), 3), Leaf('m', 1), List('T', 'e', 'm'), 4), Leaf('o', 1), List('T', 'e', 'm', 'o'), 5), Leaf('s', 1), List('T', 'e', 'm', 'o', 's'), 6), Leaf('t', 1), List('T', 'e', 'm', 'o', 's', 't'), 7), Leaf('x', 1), List('T', 'e', 'm', 'o', 's', 't', 'x'), 8)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a', 'b', 'd'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times(List('a', 'b', 'a'))") {
    assert(times(List('a', 'b', 'a')) === List(('a', 2), ('b', 1)))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }

  test("encode char list") {
    new TestTrees {
      assert(encode(t1)("ab".toList) === List(0, 1))
    }
  }

  test("decode bit list") {
    new TestTrees {
      assert(decode(t1, List(0, 1)) === "ab".toList)
    }
  }

  test("decode bcd from bit list") {
    new TestTrees {
      assert(decode(test1, (encode(test1)("bcd".toList))) === "bcd".toList)
    }
  }

  test("encode test tree string 'bcd' bit list") {
    new TestTrees {
      assert(encode(test1)("bcd".toList) === List(1, 0, 0, 1, 0, 1, 1, 1))
    }
  }
  new TestTrees {
    println(createCodeTree("sometext".toList))
    println(encode(frenchCode)("sometext".toList))
    println(quickEncode(frenchCode)("sometext".toList))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("check quick encoding") {
    new TestTrees {
      assert(quickEncode(test1)("bcd".toList) === encode(test1)("bcd".toList))
    }
  }
  
  test("decode and encode a very short text with a larger tree"){
    assert(decode(frenchCode, quickEncode(frenchCode)("sometext".toList)) === "sometext".toList)
  }
  
  test("encode a very short text with a larger tree"){
    assert(encode(frenchCode)("sometext".toList) === quickEncode(frenchCode)("sometext".toList))
  }
}
