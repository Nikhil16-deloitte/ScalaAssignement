package exercises

import java.io.FileNotFoundException
import java.util.Date

import java.io.FileReader


object Assignment extends App {

  //  val reader = new CSVReader(new FileReader("imdb_movies.csv"))
  //  val data = reader.readAll()
  //  val filteredData = data.filter(row => row(1) == "D.W. Griffith" && row(6).toInt >= 2010 && row(6).toInt <= 2020)
  //case class Data(imdb_title_id: String, title: String, original_title: String, year: Int, date_published : String, genre: String, duration: Int, country: String, language: String, director: String, writer: String, production_company: String, actors: String, description : String, avg_votes : Double, votes : Int, budget : String)
  //object MovieDataReader extends App{
  //
  //  val source = scala.io.Source.fromFile("imdb_movies.csv")
  //  val lines = source.getLines().take(10001).drop(1)
  //
  //  val movieData= lines.map { line =>
  //    val p = line.split(",(?=([^\"]*\"[^\"]*\")*[^\"]*$)")
  //    // println(line)
  //
  //    println(p(0) +"--"+ p(1) +"--"+ p(2) +"--"+p(3)+"--"+p(4)+"--"+p(5)+"--"+p(6) +"--"+ p(7) +"--"+ p(8) +"--"+p(9)+"--"+p(10)+"--"+p(11)+"--"+p(12) +"--"+ p(13) +"--"+ p(14) +"--"+p(15)+"--"+p(16))
  //    Data(p(0).toString, p(1).toString, p(2).toString, p(3).toInt, p(4).toString,p(5).toString, p(6).toInt ,p(7).toString, p(8).toString, p(9).toString, p(10).toString, p(11).toString, p(12).toString, p(13).toString, p(14).toDouble, p(15).toInt, p(16).toString)
  //
  //  }.toArray
  //
  //  source.close()
  //
  //  movieData.take(5) foreach println
  //  println(movieData(2).budget)
  //  println(movieData.length)
  //  //
  //  // val number = List.range(0,10000)
  //  // for {
  //  // n <- number
  //  // } println(movieData(n).year)
  //
  //  // def gettitles(director: String) : List[String] = {
  //  // val number = List.range(0,10000)
  //  // for {
  //  // n <- number
  //  // } movieData(n).
  //  //
  //  // }
  //
  //
  //
  //}
  def readCSV(path: String): Option[List[Map[String, String]]] = {
    try {
      val csvFile = scala.io.Source.fromFile(path)

      // read the head row
      val headRow = csvFile.getLines().next().split(",", -1)

      // read the row data
      val dataRows = csvFile.getLines().drop(1).take(10000).map { line =>
        // split each row into columns and handle cases where a column value contains commas (through regex exp)
        val columns = line.split(",(?=([^\"]*\"[^\"]*\")*[^\"]*$)", -1).map(_.replaceAll("\"", ""))

        // create a mapping of column name to value
        headRow.zip(columns).toMap

      }.toList

      csvFile.close()

      Some(dataRows)
    } catch {
      case e: FileNotFoundException => {
        println(s"Error: Could not find csv file at path: $path")
        None
      }
      case e: Exception => {
        println(s"Error message: ${e.getMessage}")
        None
      }
    }
  }

  val dataRows = readCSV("imdb_movies.csv")
  println("Answer 1***********************************************************")
  searchByDirectorAndYearRange("D.W. Griffith", 2010, 2020, dataRows)
  println()
  println("Answer 2***********************************************************")
  searchByLanguageAndUserReview("English", 8.4, dataRows)
  println()
  println("Answer 3***********************************************************")
  searchByYearAndCountryForBudget(1912, "USA", dataRows)
  println()
  println("Answer 4***********************************************************")
  searchByLanguageDurationAndVotes("English", 8.5, dataRows)
  println()
  println("Answer 5***********************************************************")
  searchByLanguageAndBudgetRange("English", 15000, 20000, dataRows)

  def searchByDirectorAndYearRange(director: String, startYear: Int, endYear: Int, dataRows: Option[List[Map[String, String]]]): Unit = {

    dataRows match {
      case Some(rows) =>
        val matchingRows = rows.filter(row => {
          val directors = row.getOrElse("director", "")
          val yearString = row.getOrElse("year", "")
          if (directors.contains(director)) {

            if (yearString.nonEmpty) {

              val year = yearString.toInt
              year >= startYear && year <= endYear
            } else {
              false
            }
          } else {
            false
          }
        })

        // Print the matching rows as a report
        if (matchingRows.nonEmpty) {
          println("Titles directed by the director" + director + " in the year range of " + startYear + " to " + endYear + "are:")
          println("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
          matchingRows.foreach(row => {
            println(row("title") + " (" + row("year") + ")")
          })
        } else {
          println("No titles directed by " + director + " in the year range of" + startYear + " to " + endYear + " were found.")
        }

      case None => println("Error: Data is missing.")
    }
  }

  def searchByLanguageAndUserReview(lang: String, userReview: Double, dataRows: Option[List[Map[String, String]]]): Unit = {

    dataRows match {
      case Some(rows) =>
        val matchingRows = rows.filter(row => {
          val language = row.getOrElse("language", "")
          val review = row.getOrElse("avg_vote", "")
          if (language.contains(lang)) {

            if (review.nonEmpty) {

              val userRev = review.toDouble
              userRev >= userReview
            } else {
              false
            }
          } else {
            false
          }
        })

        // Print the matching rows as a report
        if (matchingRows.nonEmpty) {
          println("Titles in " + lang + " with avg_rating greater than " + userReview + "in descending order:")
          println("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
          matchingRows.sortBy(_("avg_vote").toDouble).reverse.foreach(row => {
            println(row("title") + " (" + row("avg_vote") + ")")
          })

        } else {
          println("No movie titles in " + lang + " with rating greater than " + userReview + " were found.")
        }

      case None => println("Error: Data is missing.")
    }
  }

  def searchByYearAndCountryForBudget(givenYear: Int, givenCountry: String, dataRows: Option[List[Map[String, String]]]): Unit = {

    dataRows match {
      case Some(rows) =>
        val matchingRows = rows.filter(row => {
          val year = row.getOrElse("year", "")
          val country = row.getOrElse("country", "")
          val budget = row.getOrElse("budget", "")
          if (country.contains(givenCountry)) {

            if (year == givenYear.toString) {

              !budget.isEmpty
            } else {
              false
            }
          } else {
            false
          }
        })

        // Print the matching rows as a report
        if (matchingRows.nonEmpty) {
          println("Titles in " + givenCountry + " in year " + givenYear + ":")
          println("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
          matchingRows.sortBy(_("budget").filter(_.isDigit).toDouble).reverse.foreach(row => {
            println(row("title") + " (" + row("country") + ")" + " (" + row("year") + ")" + " (" + row("budget") + ")")
          })
          println()
          println("Highest budget movie in " + givenCountry + " in the year "+givenYear + " movie title: "+matchingRows.sortBy(_("budget").filter(_.isDigit).toDouble).reverse(0).get("title"))

        } else {
          println("No movie titles of " + givenCountry + " in year " + givenYear + " were found.")
        }

      case None => println("Error: Data is missing.")
    }

  }
  def searchByLanguageAndBudgetRange(lang: String, startBudget: Int, endBudget: Int, dataRows: Option[List[Map[String, String]]]): Unit = {

    dataRows match {
      case Some(rows) =>
        val matchingRows = rows.filter(row => {
          val language = row.getOrElse("language", "")
          val budget = row.getOrElse("budget", "")
          if (language.nonEmpty) {

            if (budget.nonEmpty) {

              val budg = budget.filter(_.isDigit).toInt
              budg >= startBudget && budg <= endBudget
            } else {
              false
            }
          } else {
            false
          }
        })

        // Print the matching rows as a report
        if (matchingRows.nonEmpty) {
          println("Titles in the budget range " + startBudget + " to " + endBudget + ":")
          println("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
         matchingRows.groupBy(_("language"))
            .foreach(row => {
              println("Language: " + row._1 +" and No. of movie titles: "+ row._2.size)
//            println(row("language") ++" (" + row("title") + ")" + " (" + row("budget") + ")")
          })
          println()
          println("Total Count:"+matchingRows.size)
        } else {
          println("No titles in " + lang + " in the budget range " + startBudget + " to " + endBudget + " were found.")
        }

      case None => println("Error: Data is missing.")
    }
  }

  def searchByLanguageDurationAndVotes(lang: String, minmVote : Double, dataRows: Option[List[Map[String, String]]]): Unit = {

    dataRows match {
      case Some(rows) =>
        val matchingRows = rows.filter(row => {
          val language = row.getOrElse("language", "")

          val votes = row.getOrElse("avg_vote","")
          if (language.contains(lang)) {

            if (votes.nonEmpty) {

              votes.toDouble > minmVote

            } else {
              false
            }
          } else {
            false
          }
        })

        // Print the matching rows as a report
        if (matchingRows.nonEmpty) {
          println("Titles in " + lang + " with avg_rating greater than " + minmVote + " and in reverse order of duration:")
          println("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
          matchingRows.sortBy(_("duration").toDouble).reverse.foreach(row => {
            println(row("title") +  " (" + row("language") + ")"+" (" + row("avg_vote") + ")"+ " (" + row("duration") + ")")
          })
          println()
          println("Highest duration movie title: "+matchingRows.sortBy(_("duration").toDouble).reverse(0).get("title"))
        } else {
          println("No movie titles in " + lang + " with rating greater than " + minmVote + " were found.")
        }

      case None => println("Error: Data is missing.")
    }
  }


}
