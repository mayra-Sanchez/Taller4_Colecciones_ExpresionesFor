import Anagramas.{anagramasDeFrase, anagramasDePalabras, combinaciones, complemento, diccionarioPorOcurrencias, lOcPal, locFrase}

/**
 * Palabras
 */
val palabra =lOcPal("japones")

combinaciones(palabra)
anagramasDePalabras("japones")

val palabra2 =lOcPal("delira")

combinaciones(palabra2)
anagramasDePalabras("delira")

val palabra3 =lOcPal("matar")

combinaciones(palabra3)
anagramasDePalabras("matar")

val palabra4 =lOcPal("sopa")

combinaciones(palabra4)
anagramasDePalabras("sopa")

val palabra5 =lOcPal("agranda")

combinaciones(palabra5)
anagramasDePalabras("agranda")

//complementoS
complemento(lOcPal("japones"), lOcPal("es"))
complemento(lOcPal("delira"), lOcPal("ira"))
complemento(lOcPal("matar"), lOcPal("tar"))
complemento(lOcPal("sopa"), lOcPal("a"))
complemento(lOcPal("agranda"), lOcPal("da"))


/**
 * Frases
 */

val frase= List ( "cosas","como","yo")

locFrase(frase)
anagramasDeFrase(frase)
combinaciones(locFrase(frase))

val frase2= List ( "yo","amo","jugar")

locFrase(frase2)
anagramasDeFrase(frase2)
combinaciones(locFrase(frase2))

val frase3= List ( "me","gusta","salir")

locFrase(frase3)
anagramasDeFrase(frase3)
combinaciones(locFrase(frase3))

val frase4= List ( "amo","la","programacion")

locFrase(frase4)
anagramasDeFrase(frase4)
combinaciones(locFrase(frase4))

val frase5= List ( "quiero","viajar","mucho")

locFrase(frase5)
anagramasDeFrase(frase5)
combinaciones(locFrase(frase5))

diccionarioPorOcurrencias






