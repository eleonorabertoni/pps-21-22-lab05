package u05lab.ex2

trait ConferenceReviewing:
  def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit
  def loadReview(article: Int, scores: Map[Question, Int]): Unit
  def orderedScores(article: Int, question: Question): List[Int]
  def averageFinalScore(article: Int): Double
  def acceptedArticles(): Set[Int]
  def sortedAcceptedArticles(): List[(Int, Double)]
  def averageWeightedFinalScoreMap(): Map[Int, Double]

enum Question:
  case RELEVANCE, SIGNIFICANCE, CONFIDENCE, FINAL

case class ConferenceReviewingImpl() extends ConferenceReviewing:
  var articleReview: List[(Int, Map[Question, Int])] = List.empty

  override def loadReview(article: Int, scores: Map[Question, Int]): Unit = articleReview = (article, scores) :: articleReview

  override def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit =
    loadReview(article, Map(Question.RELEVANCE -> relevance, Question.SIGNIFICANCE -> significance, Question.CONFIDENCE -> confidence, Question.FINAL -> fin))

  override def orderedScores(article: Int, question: Question): List[Int] = articleReview.filter(e => e._1 == article).map(e => e._2(question)).sorted

  override def averageFinalScore(article: Int): Double =
    val temp =  orderedScores(article, Question.FINAL)
    temp.sum / temp.length.toDouble

  override def acceptedArticles(): Set[Int] =
    articleReview.map(_._1).filter(a => averageFinalScore(a) >= 5 && isRelevant(a)).toSet

  override def sortedAcceptedArticles(): List[(Int, Double)] = acceptedArticles().map(a => (a, averageFinalScore(a))).toList.sorted((a, b) => (a._2 - b._2).toInt)

  override def averageWeightedFinalScoreMap(): Map[Int, Double] =
    articleReview.map(e => e._1).map(e => (e, averageWeightedFinalScore(e))).toMap

  private def averageWeightedFinalScore(article: Int) =
    val temp = articleReview.filter(e => e._1 == article).map(a => a._2(Question.CONFIDENCE) * a._2(Question.FINAL) / 10.0)
    temp.sum / temp.length.toDouble

  private def isRelevant(article: Int): Boolean = orderedScores(article, Question.RELEVANCE).exists(_ >= 8)
