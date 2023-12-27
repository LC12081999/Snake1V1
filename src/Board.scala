class Board(val WIDTH: Int = 25, val HEIGHT: Int = 25) {
  var gameOver: Boolean = false
  var board: Array[Array[Square]] = Array.ofDim(HEIGHT, WIDTH)
  for (i: Int <- 0 until HEIGHT) {
    for (j: Int <- 0 until HEIGHT) {
      board(i)(j) = new Square(0, 'z')
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
    var i: Int = (math.random() * 25).toInt
    var j: Int = (math.random() * 25).toInt
    if (board(i)(j).player == 'z') board(i)(j) = new Square(-1, 'z')
    else spawnFood()
  }

  def deleteLastPart(length: Int): Unit = {
    for (i: Int <- 0 until board.length) {
      for (j: Int <- 0 until board(0).length) {
        if (board(i)(j).snakePos == length) board(i)(j) = new Square(0, 'z')
      }
    }
  }

  def movement(c: Char, s: String): Unit = {
    val length: Int = getSnakeLength(c) + 1
    for (i: Int <- 0 until board.length) {
      for (j: Int <- 0 until board(0).length) {
        if (board(i)(j).player == c) {
          board(i)(j).snakePos += 1
        }
      }
    }
    for (i: Int <- 0 until board.length) {
      for (j: Int <- 0 until board(0).length) {
        if (board(i)(j).snakePos == 2 && board(i)(j).player == c) {
          s match {
            case "up" => if (i > 0 && board(i - 1)(j).player == 'z' && board(i - 1)(j).snakePos == 0) {
              board(i - 1)(j) = new Square(1, c)
              deleteLastPart(length)
            } else if (i > 0 && board(i - 1)(j).player == 'z' && board(i - 1)(j).snakePos == -1) {
              board(i - 1)(j) = new Square(1, c)
              spawnFood()
            } else if (i > 0 && board(i - 1)(j).player != 'z') gameOver = true
            else if (i == 0) gameOver = true

            case "down" => if (i < HEIGHT - 1 && board(i + 1)(j).player == 'z' && board(i + 1)(j).snakePos == 0) {
              board(i + 1)(j) = new Square(1, c)
              deleteLastPart(length)
            } else if (i < HEIGHT - 1 && board(i + 1)(j).player == 'z' && board(i + 1)(j).snakePos == -1) {
              board(i + 1)(j) = new Square(1, c)
              spawnFood()
            } else if (i < HEIGHT - 1 && board(i + 1)(j).player != 'z') gameOver = true
            else if (i == HEIGHT - 1) gameOver = true

            case "right" => if (j < WIDTH - 1 && board(i)(j + 1).player == 'z' && board(i)(j + 1).snakePos == 0) {
              board(i)(j + 1) = new Square(1, c)
              deleteLastPart(length)
            } else if (j < WIDTH - 1 && board(i)(j + 1).player == 'z' && board(i)(j + 1).snakePos == -1) {
              board(i)(j + 1) = new Square(1, c)
              spawnFood()
            } else if (j < WIDTH - 1 && board(i)(j + 1).player != 'z') gameOver = true
            else if (j == WIDTH - 1) gameOver = true

            case "left" => if (j > 0 && board(i)(j - 1).player == 'z' && board(i)(j - 1).snakePos == 0) {
              board(i)(j - 1) = new Square(1, c)
              deleteLastPart(length)
            } else if (j > 0 && board(i)(j - 1).player == 'z' && board(i)(j - 1).snakePos == -1) {
              board(i)(j - 1) = new Square(1, c)
              spawnFood()
            } else if (j > 0 && board(i)(j - 1).player != 'z') gameOver = true
            else if (j == 0) gameOver = true
          }
        }
      }
    }
  }
}
