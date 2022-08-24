import scala.collection.SortedMap
import scala.collection.immutable._

package object Anagramas {
  /**
   * Representación
   */
  type Palabra = String /* Una palabra sera una cadena sin espacios en blanco ni signos de puntuacion, ni acentos,
  formada con las letras del alfabeto en minusculas */

  type Frase = List[Palabra] // Representacion de que una frase es una lista de palabras

  type Ocurrencias = List [(Char, Int)] /*  Una lista de ocurrencias de una frase o de una palabra, es una lista de parejas (car, num) ordenada
  por car, donde num es un entero positivo que indica cuantas veces aparece el caracter car en la frase o en la palabra */

  /* El diccionario de las palabras con respecto a las cuales se haran los anagramas, se representara por medio de una lista de palabras */
  val diccionario: List[Palabra] = {
    List("japones","delira","matar","sopa","agranda","esponja","lidera","granada","paso","marta","yo","como","cosas","amo","la","programacion","gusta","me","salir","quiero","viajar","mucho","jugar")
  }

  /**
   * Calculando lista de ocurrencias
   */
  // (lista de ocurrencias de una palabra) recibe una palabra, y devuelve la lista de ocurrencias asociada a esa palabra:
  def lOcPal(p: Palabra): Ocurrencias = {
    val asociacion = (p.toLowerCase groupBy identity) map{case (w,ws)=>(w,ws.length)}
    (SortedMap[Char,Int]() ++ asociacion).toList
  }

  // (lista de ocurrencias de una frase) recibe una frase, y devuelve la lista de ocurrencias asociada a esa frase
  def locFrase (f: Frase): Ocurrencias = { lOcPal(f.mkString)}

  /**
   * Calculando los anagramas de una palabra
   */
  // agrupa las palabras del diccionario por sus listas de ocurrencias
  lazy val diccionarioPorOcurrencias: Map [Ocurrencias, List[Palabra]] ={
  diccionario groupBy (palabra => lOcPal(palabra))
  }

  // dada una palabra devuelve la lista de sus anagramas, usando el valor diccionarioPorOcurrencias
  def anagramasDePalabras (pal: Palabra): List[Palabra]={
    diccionarioPorOcurrencias.apply(lOcPal(pal))
  }

  /**
   * Calculando los subconjuntos de un conjunto con repeticiones
   */
  /* La funcion combinaciones recibe una lista de ocurrencias, y devuelve la lista de todas las sublistas de ocurrencias de la lista original (incluyendo la sublista sin caracteres)*/
  def combinaciones (lOcurrencias: Ocurrencias): List[Ocurrencias] = {
    (lOcurrencias foldRight List[Ocurrencias](Nil)) { case ((a, b), c) => {
      c ++ (for {combinar <- c; n <- 1 to b} yield (a, n) :: combinar)
    }
    }
  }

  /**
   * Calculando los anagramas de una frase
   */
  /*
  la funcion complemento que dadas una lista de ocurrencias lOc y una sublista de de esa lista slOc, devuelve la sublista de ocurrencias complementaria de slOc, es decir la sublista de ocurrencias que tiene los caracteres de la lista lOc que no estan en slOc
   */
  def complemento(lOc: Ocurrencias, slOc: Ocurrencias): Ocurrencias = {
    def auxComplemento(lOc1: Ocurrencias, slOc1: Ocurrencias): Ocurrencias = slOc1 match {
    case Nil => lOc1
    case y :: yy =>
    val (xChar, xInt) = lOc1.unzip
    val (yChar, yInt) = slOc1.unzip
    val index = xChar.indexOf(yChar.head)
    val xNew: List[(Char, Int)] = xChar.zip(xInt.updated(index, xInt(index) - yInt.head))
      auxComplemento(xNew, yy)
    }
    auxComplemento(lOc, slOc) flatMap dropZero
  }

  def dropZero(elem: (Char, Int)): List[(Char, Int)] = elem match {
  case (char, 0) => Nil
  case (char, int) => List((char, int))}

  /*def complemento (lOc: Ocurrencias , slOc: Ocurrencias): Ocurrencias = {
    def complemento1(lOc1: Ocurrencias, slOc2: Ocurrencias): Ocurrencias = slOc2 match {
      case Nil => lOc1
      case slOc :: slOcss =>
        val (lOcChar, lOcInt) = lOc1.unzip
        val (slOcChar, slOcInt) = slOc2.unzip
        val index = lOcChar.indexOf(slOcChar.head)
        val lOcNew: List[(Char, Int)] = lOcChar.zip(lOcInt.updated(index, lOcInt(index) - slOcInt.head))
        complemento1(lOcNew, slOcss)
    }
    complemento1(lOc, slOc) flatMap reemplazar
  }

  def reemplazar (elem: (Char, Int)): List[(Char, Int)] = elem match {
    case (char, 0) => Nil
    case (char, int) => List((char, int))}
*/
  // la funcion anagramasDeFrase, que recibe una frase, y devuelve la lista de frases que son anagramas de la frase original
  def anagramasDeFrase(frase: Frase): List[Frase] = {
    def iteracionFor(lOcurrencias: Ocurrencias): List[Frase] = {
      if (lOcurrencias.isEmpty) List(Nil)
      else for {
        combinacion <- combinaciones( lOcurrencias )
        palabra <- diccionarioPorOcurrencias getOrElse (combinacion, Nil)
        frase <- iteracionFor( complemento(lOcurrencias,lOcPal(palabra)) )
        if !combinacion.isEmpty
      } yield palabra :: frase
    }
    iteracionFor( locFrase(frase) )
  }
}
