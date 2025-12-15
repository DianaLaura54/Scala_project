import scala.io.StdIn
import scala.util.{Random, Try}
import scala.io.Source

object WordleAdvanced {
  

  object Colors {
    val GREEN = "\u001b[42m\u001b[30m"
    val YELLOW = "\u001b[43m\u001b[30m"
    val GRAY = "\u001b[47m\u001b[30m"
    val RESET = "\u001b[0m"
    val BOLD = "\u001b[1m"
    val BLUE = "\u001b[34m"
    val RED = "\u001b[31m"
  }
  
  
  case class GameStats(
    gamesPlayed: Int = 0,
    gamesWon: Int = 0,
    currentStreak: Int = 0,
    maxStreak: Int = 0,
    guessDistribution: Map[Int, Int] = Map(1 -> 0, 2 -> 0, 3 -> 0, 4 -> 0, 5 -> 0, 6 -> 0)
  ) {
    def winRate: Double = if (gamesPlayed == 0) 0.0 else (gamesWon.toDouble / gamesPlayed) * 100
    
    def averageGuesses: Double = {
      if (gamesWon == 0) 0.0
      else {
        val totalGuesses = guessDistribution.map { case (guesses, count) => guesses * count }.sum
        totalGuesses.toDouble / gamesWon
      }
    }
    
    def recordWin(attempts: Int): GameStats = {
      this.copy(
        gamesPlayed = gamesPlayed + 1,
        gamesWon = gamesWon + 1,
        currentStreak = currentStreak + 1,
        maxStreak = math.max(maxStreak, currentStreak + 1),
        guessDistribution = guessDistribution.updated(attempts, guessDistribution(attempts) + 1)
      )
    }
    
    def recordLoss(): GameStats = {
      this.copy(
        gamesPlayed = gamesPlayed + 1,
        currentStreak = 0
      )
    }
  }
  

  sealed trait Difficulty {
    def maxAttempts: Int
    def hintsAvailable: Int
    def name: String
  }
  
  case object Easy extends Difficulty {
    val maxAttempts = 8
    val hintsAvailable = 3
    val name = "Easy"
  }
  
  case object Medium extends Difficulty {
    val maxAttempts = 6
    val hintsAvailable = 2
    val name = "Medium"
  }
  
  case object Hard extends Difficulty {
    val maxAttempts = 5
    val hintsAvailable = 1
    val name = "Hard"
  }
  
  case object Expert extends Difficulty {
    val maxAttempts = 4
    val hintsAvailable = 0
    val name = "Expert"
  }
  
 
  object WordList {
    val animals = List("TIGER", "HORSE", "WHALE", "EAGLE", "PANDA", "SHARK", "SNAKE", "BEARS", "ZEBRA", "KOALA")
    val nature = List("BEACH", "RIVER", "CLOUD", "STONE", "PLANT", "OCEAN", "STORM", "GRASS", "FLAME", "EARTH")
    val food = List("PIZZA", "BREAD", "PASTA", "APPLE", "MANGO", "BERRY", "CREAM", "SUGAR", "LEMON", "MELON")
    val tech = List("SCALA", "CODES", "BYTES", "LOGIC", "ARRAY", "NODES", "CLOUD", "QUERY", "STACK", "GRAPH")
    val music = List("MUSIC", "PIANO", "DANCE", "OPERA", "BLUES", "DRUMS", "TEMPO", "CHORD", "SCALE", "NOTES")
    val general = List("BRAIN", "LIGHT", "SPACE", "HOUSE", "WORLD", "VOICE", "GHOST", "TOWER", "MAGIC", "CROWN")
    
    val all: List[String] = animals ++ nature ++ food ++ tech ++ music ++ general
    
    def getByCategory(category: String): List[String] = category.toLowerCase match {
      case "animals" => animals
      case "nature" => nature
      case "food" => food
      case "tech" => tech
      case "music" => music
      case "general" => general
      case _ => all
    }
  }
  

  case class GameState(
    targetWord: String,
    difficulty: Difficulty,
    attempts: Int = 0,
    hintsUsed: Int = 0,
    guessHistory: List[String] = List.empty,
    letterStatuses: Map[Char, String] = Map.empty 
  )
  

  class WordleGame(var stats: GameStats) {
    
    def play(): Unit = {
      printWelcome()
      
      var playing = true
      while (playing) {
        val difficulty = selectDifficulty()
        val category = selectCategory()
        val targetWord = selectWord(category)
        
        val gameState = playRound(targetWord, difficulty)
        
        print("\nPlay again? (y/n): ")
        playing = StdIn.readLine().trim.toLowerCase.startsWith("y")
      }
      
      println("\nFinal Statistics:")
      displayStats()
      println("\nThanks for playing!")
    }
    
    def printWelcome(): Unit = {
      println("====================================")
      println("     WORDLE ADVANCED v2.0          ")
      println("====================================")
      println("Guess the 5-letter word!")
      println(s"${Colors.GREEN} GREEN ${Colors.RESET} = correct letter in correct position")
      println(s"${Colors.YELLOW} YELLOW ${Colors.RESET} = correct letter in wrong position")
      println(s"${Colors.GRAY} GRAY ${Colors.RESET} = letter not in word\n")
    }
    
    def selectDifficulty(): Difficulty = {
      println("\nSelect difficulty:")
      println("1. Easy (8 attempts, 3 hints)")
      println("2. Medium (6 attempts, 2 hints)")
      println("3. Hard (5 attempts, 1 hint)")
      println("4. Expert (4 attempts, no hints)")
      print("Choice (1-4): ")
      
      StdIn.readLine().trim match {
        case "1" => Easy
        case "2" => Medium
        case "3" => Hard
        case "4" => Expert
        case _ => 
          println("Invalid choice, defaulting to Medium")
          Medium
      }
    }
    
    def selectCategory(): String = {
      println("\nSelect word category:")
      println("1. Animals")
      println("2. Nature")
      println("3. Food")
      println("4. Technology")
      println("5. Music")
      println("6. General")
      println("7. All (random)")
      print("Choice (1-7): ")
      
      StdIn.readLine().trim match {
        case "1" => "animals"
        case "2" => "nature"
        case "3" => "food"
        case "4" => "tech"
        case "5" => "music"
        case "6" => "general"
        case _ => "all"
      }
    }
    
    def selectWord(category: String): String = {
      val wordList = WordList.getByCategory(category)
      wordList(Random.nextInt(wordList.length))
    }
    
    def playRound(targetWord: String, difficulty: Difficulty): GameState = {
      println(s"\n${Colors.BLUE}Difficulty: ${difficulty.name}${Colors.RESET}")
      println(s"${Colors.BLUE}Attempts: ${difficulty.maxAttempts} | Hints: ${difficulty.hintsAvailable}${Colors.RESET}\n")
      
      var state = GameState(targetWord, difficulty)
      var won = false
      
      while (state.attempts < difficulty.maxAttempts && !won) {
        println(s"${Colors.BOLD}Attempt ${state.attempts + 1}/${difficulty.maxAttempts}${Colors.RESET}")
        displayKeyboard(state.letterStatuses)
        
        print("Enter guess (or 'hint' for a hint, 'stats' for statistics): ")
        val input = StdIn.readLine().toUpperCase.trim
        
        input match {
          case "HINT" if state.hintsUsed < difficulty.hintsAvailable =>
            state = provideHint(state)
            
          case "HINT" =>
            println(s"${Colors.RED}No hints remaining!${Colors.RESET}")
            
          case "STATS" =>
            displayStats()
            
          case guess if guess.length != 5 =>
            println(s"${Colors.RED}Please enter a 5-letter word!${Colors.RESET}")
            
          case guess if !guess.forall(_.isLetter) =>
            println(s"${Colors.RED}Please use only letters!${Colors.RESET}")
            
          case guess if !isValidWord(guess) =>
            println(s"${Colors.RED}'$guess' is not in the word list!${Colors.RESET}")
            
          case guess =>
            state = state.copy(
              attempts = state.attempts + 1,
              guessHistory = state.guessHistory :+ guess
            )
            
            val (feedback, newLetterStatuses) = getFeedback(guess, targetWord, state.letterStatuses)
            state = state.copy(letterStatuses = newLetterStatuses)
            
            println(feedback)
            
            if (guess == targetWord) {
              won = true
              println(s"\n${Colors.GREEN}${Colors.BOLD} Congratulations! You won in ${state.attempts} attempts!${Colors.RESET}")
              println(s"The word was: ${Colors.BOLD}$targetWord${Colors.RESET}")
              stats = stats.recordWin(state.attempts)
              displayQuickStats()
            } else if (state.attempts == difficulty.maxAttempts) {
              println(s"\n${Colors.RED}${Colors.BOLD} Game Over!${Colors.RESET}")
              println(s"The word was: ${Colors.BOLD}$targetWord${Colors.RESET}")
              stats = stats.recordLoss()
            }
            println()
        }
      }
      
      state
    }
    
    def isValidWord(guess: String): Boolean = {
     
      WordList.all.contains(guess)
    }
    
    def provideHint(state: GameState): GameState = {
      val revealPosition = Random.nextInt(5)
      val letter = state.targetWord(revealPosition)
      println(s"${Colors.BLUE}Hint: Position ${revealPosition + 1} is '${letter}'${Colors.RESET}")
      state.copy(hintsUsed = state.hintsUsed + 1)
    }
    
    def getFeedback(guess: String, target: String, currentLetterStatuses: Map[Char, String]): (String, Map[Char, String]) = {
      val targetChars = target.toList
      val result = new StringBuilder
      var updatedStatuses = currentLetterStatuses
      
      val targetUsed = Array.fill(5)(false)
      val guessStatus = Array.fill(5)("")
      
     
      for (i <- 0 until 5) {
        if (guess(i) == target(i)) {
          guessStatus(i) = Colors.GREEN
          targetUsed(i) = true
          updatedStatuses = updatedStatuses.updated(guess(i), Colors.GREEN)
        }
      }
      
   
      for (i <- 0 until 5) {
        if (guessStatus(i).isEmpty) {
          val char = guess(i)
          val foundIndex = targetChars.indices.find(j => 
            targetChars(j) == char && !targetUsed(j) && j != i
          )
          
          if (foundIndex.isDefined) {
            guessStatus(i) = Colors.YELLOW
            targetUsed(foundIndex.get) = true
            if (updatedStatuses.get(char) != Some(Colors.GREEN)) {
              updatedStatuses = updatedStatuses.updated(char, Colors.YELLOW)
            }
          } else {
            guessStatus(i) = Colors.GRAY
            if (!updatedStatuses.contains(char)) {
              updatedStatuses = updatedStatuses.updated(char, Colors.GRAY)
            }
          }
        }
      }
      
      
      for (i <- 0 until 5) {
        result.append(s"${guessStatus(i)} ${guess(i)} ${Colors.RESET}")
      }
      
      (result.toString, updatedStatuses)
    }
    
    def displayKeyboard(letterStatuses: Map[Char, String]): Unit = {
      val rows = List(
        "QWERTYUIOP",
        "ASDFGHJKL",
        "ZXCVBNM"
      )
      
      println("\nKeyboard:")
      rows.foreach { row =>
        print("  ")
        row.foreach { letter =>
          val color = letterStatuses.getOrElse(letter, "")
          print(s"$color $letter ${Colors.RESET}")
        }
        println()
      }
      println()
    }
    
    def displayStats(): Unit = {
      println(s"\n${Colors.BOLD}═══ Statistics ═══${Colors.RESET}")
      println(s"Games Played: ${stats.gamesPlayed}")
      println(s"Win Rate: ${stats.winRate}%.2f%%")
      println(s"Current Streak: ${stats.currentStreak}")
      println(s"Max Streak: ${stats.maxStreak}")
      println(f"Average Guesses: ${stats.averageGuesses}%.2f")
      
      println(s"\n${Colors.BOLD}Guess Distribution:${Colors.RESET}")
      for (i <- 1 to 6) {
        val count = stats.guessDistribution(i)
        val bar = "█" * count
        println(f"$i: $bar ($count)")
      }
      println()
    }
    
    def displayQuickStats(): Unit = {
      println(s"\n${Colors.BLUE}Streak: ${stats.currentStreak} | Win Rate: ${stats.winRate}%.1f%%${Colors.RESET}")
    }
  }
  
  def main(args: Array[String]): Unit = {
    val initialStats = GameStats()
    val game = new WordleGame(initialStats)
    game.play()
  }
}