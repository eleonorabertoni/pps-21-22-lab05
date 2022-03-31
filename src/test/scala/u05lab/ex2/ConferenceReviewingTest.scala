package u05lab.ex2

import org.junit.Test
import org.junit.Assert.*
import u05lab.ex2.ConferenceReviewing

class ConferenceReviewingTest {

  /**
   * Si consulti la documentazione dell'interfaccia ConferenceReviewing, che modella i risultati del processo di revisione
   * degli articoli di una conferenza. Ogni articolo viene da revisionato da uno o più revisori anonimi, ognuno dei quali fornisce
   * una valutazione (score) da 0 a 10 per 4 diverse "domande", modellate da ConferenceReviewing.Question. Un articolo viene
   * accettato se il valore medio della valutazione alla domanda "FINAL" è >5 e se ha almeno una valutazione "RELEVANCE" >= 8.
   *
   * Implementare ConferenceReviewing attraverso una classe ConferenceReviewingImpl con costruttore senza argomenti,
   * in modo che passi tutti i test di cui sotto, realizzati per essere autoesplicativi.
   *
   * Sono considerati opzionali ai fini della possibilità di correggere l'esercizio, ma concorrono comunque al raggiungimento
   * della totalità del punteggio:
   * - implementazione dei test opzionali (relativi alla realizzazione del metodo averageWeightedFinalScoreMap)
   * - la qualità della soluzione, in particolare con minimizzazione di ripetizioni e codice non inutilmente complesso
   */

    val cr: ConferenceReviewing = ConferenceReviewingImpl()
    // loading a revision for article 1 with grades for relevance, significance, confidence, and final
    cr.loadReview(1, 8, 8, 6, 8) // 4.8
    cr.loadReview(1, 9, 9, 6, 9) // 5.4
    cr.loadReview(2, 9, 9, 10, 9) // 9.0
    cr.loadReview(2, 4, 6, 10, 6) // 6.0
    cr.loadReview(3, 3, 3, 3, 3) // 0.9
    cr.loadReview(3, 4, 4, 4, 4) // 1.6
    cr.loadReview(4, 6, 6, 6, 6) // 3.6
    cr.loadReview(4, 7, 7, 8, 7) // 5.6


    val map: Map[Question, Int] = Map(Question.RELEVANCE -> 8, Question.SIGNIFICANCE -> 8, Question.CONFIDENCE -> 7, Question.FINAL -> 8)
    cr.loadReview(4,map)
    cr.loadReview(5, 6, 6, 6, 10); // 6.0
    cr.loadReview(5, 7, 7, 7, 10); // 7.0

    @Test
    def testOrderedScores(): Unit =
      //article number 2 received scores 4 and 9 in RELEVANCE...
      assertEquals(List(4, 9), cr.orderedScores(2, Question.RELEVANCE))
      assertEquals(List(6, 7, 8), cr.orderedScores(4, Question.CONFIDENCE))
      assertEquals(List(10, 10), cr.orderedScores(5, Question.FINAL))

    @Test
    def testAverageFinalScore(): Unit =
      //article number 1 received 8.5 as average score for FINAL with a maximum deviation of 0.01
      assertEquals(8.5, cr.averageFinalScore(1), 0.01)
      assertEquals(7.5, cr.averageFinalScore(2),0.01)
      assertEquals(3.5, cr.averageFinalScore(3),0.01)
      assertEquals(7.0, cr.averageFinalScore(4),0.01)
      assertEquals(10.0, cr.averageFinalScore(5),0.01)

    @Test
    def testAcceptedArticles(): Unit =
      //only articles 1, 2, 4 can be accepted because average score >=5 and at least one score on RELEVANCE >= 8
      assertEquals(cr.acceptedArticles(), Set(1,2,4))

    @Test
    def testSortedAcceptedArticles(): Unit =
      //accepted articles and average final score, from lower score
      assertEquals(List((4, 7.0), (2, 7.5), (1, 8.5)), cr.sortedAcceptedArticles())

    @Test
    def testAverageWeightedFinalScore(): Unit =
      // article number 1 got an average final score of (4.8+5.4)/2 = 5,1
      assertEquals((4.8+5.4)/2, cr.averageWeightedFinalScoreMap()(1), 0.01)
      assertEquals((9.0+6.0)/2, cr.averageWeightedFinalScoreMap()(2),0.01)
      assertEquals((0.9+1.6)/2, cr.averageWeightedFinalScoreMap()(3),0.01)
      assertEquals((3.6+5.6+5.6)/3, cr.averageWeightedFinalScoreMap()(4),0.01)
      assertEquals((6.0+7.0)/2, cr.averageWeightedFinalScoreMap()(5),0.01)
      assertEquals(5, cr.averageWeightedFinalScoreMap().size)

}
