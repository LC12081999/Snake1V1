class Board(val WIDTH: Int = 25, val HEIGHT: Int = 25) {
  var gameOverA: Boolean = false
  var gameOverB: Boolean = false
  var board: Array[Array[Square]] = Array.ofDim(HEIGHT, WIDTH)
  val up: AudioPlayer = new AudioPlayer("res/haut.wav")
  val down: AudioPlayer = new AudioPlayer("res/bas.wav")
  val left: AudioPlayer = new AudioPlayer("res/gauche.wav")
  val right: AudioPlayer = new AudioPlayer("res/droite.wav")
  val miam = new AudioPlayer("res/miam.wav")


  def setGrid(): Unit = {
    for (i: Int <- 0 until HEIGHT) {
      for (j: Int <- 0 until HEIGHT) {
        board(i)(j) = new Square(0, 'z')
      }
    }
  }

  def spawnPlayer(): Unit = {
    board(0)(0) = new Square(3, 'a')
    board(0)(1) = new Square(2, 'a')
    board(0)(2) = new Square(1, 'a')
    board(24)(24) = new Square(3, 'b')
    board(24)(23) = new Square(2, 'b')
    board(24)(22) = new Square(1, 'b')
  }

  def getSnakeLength(c: Char): Int = {
    var length: Int = 0
    for (i: Int <- 0 until board.length) {
      for (j: Int <- 0 until board(0).length) {
        if (board(i)(j).player == c && board(i)(j).snakePos > length) length = board(i)(j).snakePos
      }
    }
    return length
  }


  def spawnFood(): Unit = {
    miam.play()
    var i: Int = (math.random() * 25).toInt
    var j: Int = (math.random() * 25).toInt
    if (board(i)(j).player == 'z') board(i)(j) = new Square(-1, 'z')
    else spawnFood()
  }

  def deleteLastPart(player: Char, length: Int): Unit = {
    for (i: Int <- 0 until board.length) {
      for (j: Int <- 0 until board(0).length) {
        if (board(i)(j).snakePos == length && board(i)(j).player == player) board(i)(j) = new Square(0, 'z')
      }
    }
  }

  def movement(player: Char, s: String): Unit = {
    val length: Int = getSnakeLength(player) + 1
    for (i: Int <- 0 until board.length) {
      for (j: Int <- 0 until board(0).length) {
        if (board(i)(j).player == player) {
          board(i)(j).snakePos += 1
        }
      }
    }
    for (i: Int <- 0 until board.length) {
      for (j: Int <- 0 until board(0).length) {
        if (board(i)(j).snakePos == 2 && board(i)(j).player == player) {
          s match {
            case "up" => if (i > 0 && board(i - 1)(j).player == 'z' && board(i - 1)(j).snakePos == 0) {
              board(i - 1)(j) = new Square(1, player)
              deleteLastPart(player, length)
              up.play()
            } else if (i > 0 && board(i - 1)(j).player == 'z' && board(i - 1)(j).snakePos == -1) {
              board(i - 1)(j) = new Square(1, player)
              spawnFood()
              up.play()
            } else if (i > 0 && board(i - 1)(j).player != 'z') gameOver(player)
            else if (i == 0) gameOver(player)

            case "down" => if (i < HEIGHT - 1 && board(i + 1)(j).player == 'z' && board(i + 1)(j).snakePos == 0) {
              board(i + 1)(j) = new Square(1, player)
              deleteLastPart(player, length)
              down.play()
            } else if (i < HEIGHT - 1 && board(i + 1)(j).player == 'z' && board(i + 1)(j).snakePos == -1) {
              board(i + 1)(j) = new Square(1, player)
              down.play()
              spawnFood()
            } else if (i < HEIGHT - 1 && board(i + 1)(j).player != 'z') gameOver(player)
            else if (i == HEIGHT - 1) gameOver(player)

            case "right" => if (j < WIDTH - 1 && board(i)(j + 1).player == 'z' && board(i)(j + 1).snakePos == 0) {
              board(i)(j + 1) = new Square(1, player)
              deleteLastPart(player, length)
              right.play()
            } else if (j < WIDTH - 1 && board(i)(j + 1).player == 'z' && board(i)(j + 1).snakePos == -1) {
              board(i)(j + 1) = new Square(1, player)
              spawnFood()
              right.play()

            } else if (j < WIDTH - 1 && board(i)(j + 1).player != 'z') gameOver(player)
            else if (j == WIDTH - 1) gameOver(player)

            case "left" => if (j > 0 && board(i)(j - 1).player == 'z' && board(i)(j - 1).snakePos == 0) {
              board(i)(j - 1) = new Square(1, player)
              deleteLastPart(player, length)
              left.play()
            } else if (j > 0 && board(i)(j - 1).player == 'z' && board(i)(j - 1).snakePos == -1) {
              board(i)(j - 1) = new Square(1, player)
              spawnFood()
              left.play()

            } else if (j > 0 && board(i)(j - 1).player != 'z') gameOver(player)
            else if (j == 0) gameOver(player)
          }
        }
      }
    }
  }

  def gameOver(player: Char): Unit = {

    if (player == 'a') gameOverA = true
    else gameOverB = true
  }
}
