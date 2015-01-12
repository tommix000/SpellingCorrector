import java.io.File

object SpellingCorrector {
  val logger = java.util.logging.Logger.getLogger(SpellingCorrector.getClass.getName)

  /*
  def words(text): return re.findall('[a-z]+', text.lower())
   */
  def readWords(filepath: String): List[String] = {
    val file = new File(filepath)
    logger.info("reading from file " + file.getAbsolutePath)
    scala.io.Source.fromFile(file)
      .getLines
      .flatMap(_.split("\\W+")).toList.map(_.toLowerCase)
  }

  /*
  def train(features):
    model = collections.defaultdict(lambda: 1)
      for f in features:
        model[f] += 1
    return model
   */
  def train(words: List[String]): Map[String, Int] = {
    words.foldLeft(Map.empty[String, Int]) {
      (map, elem) => map.updated(elem, map.getOrElse(elem, 0) + 1)
    }
  }

  def editOp(list: List[(String, String)], minLen: Int, op: (List[String], String, String) => List[String]): List[String] = {
    list.foldLeft(List.empty[String]) {
      (list, elem) => {
        if (elem._2.length < minLen) {
          list
        } else {
          val (a, b) = elem
          op(list, a, b)
        }
      }
    }
  }

  /*
  def edits1(word):
   splits     = [(word[:i], word[i:]) for i in range(len(word) + 1)]
   deletes    = [a + b[1:] for a, b in splits if b]
   transposes = [a + b[1] + b[0] + b[2:] for a, b in splits if len(b)>1]
   replaces   = [a + c + b[1:] for a, b in splits for c in alphabet if b]
   inserts    = [a + c + b     for a, b in splits for c in alphabet]
   return set(deletes + transposes + replaces + inserts)
   */
  def edits1(word: String): List[(String)] = {
    val alphabet = 'a' to 'z' toArray
    def split(word: String, list: List[(String, String)], index: Int): List[(String, String)] = {
      if (index == word.length + 1) {
        list
      } else {
        split(word, list.+:(word.splitAt(index)), index + 1)
      }
    }
    val splits = split(word, List.empty[(String, String)], 0)

    val deletes = SpellingCorrector.editOp(splits, 1,
      (list, a, b) => a.concat(b.substring(1)) :: list)
    val transposes = SpellingCorrector.editOp(splits, 2,
      (list, a, b) => a.concat(b.substring(1, 2)).concat(b.substring(0, 1)).concat(b.substring(2)) :: list)
    val replaces = SpellingCorrector.editOp(splits, 1,
      (list, a, b) => {
        alphabet.foldLeft(list) {
          (newList, c) => a.concat(c.toString).concat(b.substring(1)) :: newList
        }
      })
    val inserts = SpellingCorrector.editOp(splits, 0,
      (list, a, b) => {
        alphabet.foldLeft(list) {
          (newList, c) => a.concat(c.toString).concat(b) :: newList
        }
      })

    deletes ::: transposes ::: replaces ::: inserts
  }

  /*
  def known(words): return set(w for w in words if w in NWORDS)
   */
  def known(wordsf: () => List[String], trained: Map[String, Int]): () => List[String] = {
    () => {
      val words = wordsf()
      logger.info(s"Looking up a list of size ${words.size} in training data")
      for (w <- words; found <- trained.get(w)) yield w
    }
  }

  /*
  def known_edits2(word):
    return set(e2 for e1 in edits1(word) for e2 in edits1(e1) if e2 in NWORDS)
    def known(words): return set(w for w in words if w in NWORDS)
   */
  def edits2(word: String): List[String] = {
    logger.info(s"$word or distance 1 variations not found in dictionary. Calculating distance 2 variations")
    edits1(word).foldLeft(List.empty[String]) {
      (list, elem) => list ::: edits1(elem)
    }
  }

  implicit def func2OrChain[A, B](f: A => Either[A, B]) = new OrChain[A, B](f)

  class OrChain[A, B](f: A => Either[A, B]) {
    def or(next: A => Either[A, B]): A => Either[A, B] =
      default => f(default).left.flatMap(next)
  }

  val orFunc =
    (f: () => List[String]) =>
      (default: List[String]) => {
        val list = f()
        if (!list.isEmpty) Right(list)
        else Left(default)
      }

  /*
  def correct(word):
  candidates = known([word]) or known(edits1(word)) or known_edits2(word) or [word]
  return max(candidates, key=NWORDS.get)
  */
  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()
    val word = args(0)
    val trainingData = train(readWords("training_data/textdata.txt"))

    val candidatesChain =
      orFunc(known(() => List(word), trainingData))
        .or(orFunc(known(() => edits1(word), trainingData)))
        .or(orFunc(known(() => edits2(word), trainingData)))

    val candidates = candidatesChain(List(word)) match {
      case Right(hits) => hits
      case Left(default) => default
    }

    val correction = candidates.foldLeft(("", -1)) {
      (max, element) => {
        val elementCount = trainingData.getOrElse(element, 0)
        if (elementCount > max._2) (element, elementCount) else max
      }
    }
    val stop = System.currentTimeMillis() - start

    println(s"WORD: $word")
    println(s"CORRECTION: ${correction._1}. Found ${correction._2} times in training data")
    println(s"Duration: $stop millis")
  }

}
