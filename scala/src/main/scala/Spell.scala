import java.io.File

object Spell {
  def correct(word: String): List[String] = {
    println(s"Looking for corrections to $word")
    val words: List[String] = dataFile.filter { w =>
      w.foldLeft(false) { (acc, char) => alphabet.contains(char) }
    }

    val start = now()
    val trainedLib = trained(words)
    val end = now()
    println(s"Training Time: ${(end - start)}ms")

    listOrElse(known(List(word), trainedLib)) orElse
    listOrElse(known(edits(word), trainedLib)) orElse
    listOrElse(known_edits2(word, trainedLib)) getOrElse List(word)
  }

  private[this] def now() = System.currentTimeMillis

  private[this] def listOrElse[A](l: Iterable[A]): Option[List[A]] =
    if (l.isEmpty) None else Some(l.toList)

  private[this] def known(words: Iterable[String], lib: List[String]): Set[String] =
    words.filter(lib.contains(_)).toSet

  private[this] def known_edits2(word: String, lib: List[String]): Set[String] =
    edits(word).flatMap(e1 => edits(e1).filter(lib.contains(_))).toSet

  private[this] def edits(word: String): List[String] = (0 to (word.length)).toList.flatMap { x =>
    val s = (word.take(x) -> word.drop(x))
    val deletes: String = s._1 + s._2.drop(1)
    val transposes: String = s._1 + s._2.drop(1).take(1) + s._2.take(1) + s._2.drop(2)
    val replaces: List[String] = alphabet.toList.map(_.toString).map { c =>
      s._1 + c + s._2.drop(1)
    }
    val inserts: List[String] = alphabet.toList.map(_.toString).map { c =>
      s._1 + c + s._2
    }

    List(word, deletes, transposes) ::: replaces ::: inserts
  }

  private[this] val alphabet = "abcdefghijklmnopqrstuvwxyz"

  private[this] lazy val trained =
    (words: Iterable[String]) => (Set.empty[String] ++ words.toSet).toList

  private[this] lazy val dataFile: List[String] =
    scala.io.Source.fromFile(new File("src/main/resources/big.txt")).mkString("").split("""\W+""").toList
}
