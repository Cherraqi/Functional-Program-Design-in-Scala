

/*** For-expression are useful for querying the database (Slick) ***/
case class Book(title: String, authors: List[String])

object queries {

  //A mini-database - implemented as a Set - GOOD
  val booksSet: Set[Book] = Set (
    Book(title 	 = "Structure and Interpretation of Computer Programs",
      authors = List("Abelson, Harald", "Sussman, Gerald J.")),
    Book(title 	 = "Introduction to Functional Programming",
      authors = List("Bird, Richard", "Wadler, Phil")),
    Book(title 	 = "Effective Java",
      authors = List("Bloch, Joshua")),
    Book(title 	 = "Effective Java 2",
      authors = List("Bloch, Joshua")),
    Book(title 	 = "Java Puzzlers",
      authors = List("Bloch, Joshua", "Gafter, Neal")),
    Book(title 	 = "Programming in Scala",
      authors = List("Ordersky, Martin", "Spoon, Lex", "Venners, Bill"))
  )

  //A mini-database - implemented as a List - BAD
  val booksList: List[Book] = List (
    Book(title 	 = "Structure and Interpretation of Computer Programs",
      authors = List("Abelson, Harald", "Sussman, Gerald J.")),
    Book(title 	 = "Introduction to Functional Programming",
      authors = List("Bird, Richard", "Wadler, Phil")),
    Book(title 	 = "Effective Java",
      authors = List("Bloch, Joshua")),
    Book(title 	 = "Effective Java 2",
      authors = List("Bloch, Joshua")),
    Book(title 	 = "Java Puzzlers",
      authors = List("Bloch, Joshua", "Gafter, Neal")),
    Book(title 	 = "Programming in Scala",
      authors = List("Ordersky, Martin", "Spoon, Lex", "Venners, Bill"))
  )

  //To find the titles of books whose author's name is "Bird"
  for(b <- booksSet; a <- b.authors if a startsWith "Bloch") yield b.title

  for(b <- booksList; a <- b.authors if a startsWith "Bloch") yield b.title


  //To find all the books which have the word "Program" in the title
  for(b <- booksSet if b.title.indexOf("Program") >= 0) yield b.title

  for(b <- booksList if b.title.indexOf("Program") >= 0) yield b.title

  booksSet.map(b => b.title indexOf "Program")


  //Find the names of all authors who have written at least two books present in the database
  for {
    b1 <- booksSet
    b2 <- booksSet
    if b1.title < b2.title
    a1 <- b1.authors
    a2 <- b2.authors
    if a1 == a2
  } yield a1
  for {
    b1 <- booksSet
    b2 <- booksSet
    if b1.title != b2.title
    a1 <- b1.authors
    a2 <- b2.authors
    if a1 == a2
  } yield a1

  for {
    b1 <- booksList
    b2 <- booksList
    if b1.title < b2.title
    a1 <- b1.authors
    a2 <- b2.authors
    if a1 == a2
  } yield a1

  for {
    b1 <- booksList
    b2 <- booksList
    if b1.title != b2.title
    a1 <- b1.authors
    a2 <- b2.authors
    if a1 == a2
  } yield a1

  for {
    b1 <- booksList
    b2 <- booksSet
    if b1.title < b2.title
    a1 <- b1.authors
    a2 <- b2.authors
    if a1 == a2
  } yield a1

  for {
    b1 <- booksSet
    b2 <- booksList
    if b1.title < b2.title
    a1 <- b1.authors
    a2 <- b2.authors
    if a1 == a2
  } yield a1


  /** Translating for expression back into HOFs **/
  for(b <- booksSet; a <- b.authors if a startsWith "Bird") yield b.title



  //substitute the first generator by flatMap
  booksSet flatMap(b =>
    for(a <- b.authors if a.startsWith("Bird")) yield b.title)

  //substitute the if-statement by withFilter
  booksSet flatMap(b =>
    for(a <- b.authors withFilter (a => a.startsWith("Bird"))) yield b.title)


  //substitute the second generator by map
  booksSet flatMap(b =>
    b.authors withFilter (a => a.startsWith("Bird")) map (t => b.title))



}