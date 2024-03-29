import hevs.graphics.FunGraphics
import hevs.graphics.utils.GraphicsBitmap

import java.awt.Color
import java.awt.event.{KeyEvent, KeyListener}

object Runner extends App {
  val death: AudioPlayer = new AudioPlayer("res/splush.wav")
  val youpi: AudioPlayer = new AudioPlayer("res/youpi.wav")
  val quit: AudioPlayer = new AudioPlayer("res/quit.wav")
  val WIDTH: Int = 500
  val HEIGHT: Int = 500
  var start: Boolean = false
  var playAgain: Boolean = false
  var end: Boolean = false
  var display: FunGraphics = new FunGraphics(WIDTH, HEIGHT)
  var playerADirection: String = "right" // direction initiale joueur a
  var playerBDirection: String = "left" // direction initiale joueur b
  var waitForInput: Boolean = false
  var startScreen: Boolean = true
  var k: KeyListener = new KeyListener {
    override def keyTyped(e: KeyEvent): Unit = {

    }

    override def keyReleased(e: KeyEvent): Unit = {

    }
    // Gestion des inputs
    override def keyPressed(e: KeyEvent): Unit = {
      e.getKeyCode match {
        case KeyEvent.VK_SPACE => if(startScreen)start = true
        case KeyEvent.VK_P => {
          if (waitForInput)
          playAgain = true
        }
        case KeyEvent.VK_Q => if (waitForInput)end = true
        case KeyEvent.VK_UP => if(playerBDirection!="down") playerBDirection = "up"
        case KeyEvent.VK_DOWN => if(playerBDirection != "up") playerBDirection = "down"
        case KeyEvent.VK_LEFT => if(playerBDirection != "right") playerBDirection = "left"
        case KeyEvent.VK_RIGHT => if(playerBDirection != "left") playerBDirection = "right"
        case KeyEvent.VK_W => if(playerADirection != "down") playerADirection = "up"
        case KeyEvent.VK_S => if(playerADirection != "up") playerADirection = "down"
        case KeyEvent.VK_D => if(playerADirection != "left") playerADirection = "right"
        case KeyEvent.VK_A => if(playerADirection != "right") playerADirection = "left"
        case _ => {
        }
      }
    }
  }
  var grid: Board = new Board()

  // Ecran d'accueil
  def startDisplay(): Unit = {
    display.clear(Color.yellow)
    display.drawFancyString(100, 200, "Press space to start the game", Color.black, 20)
  }

  // Ecran de fin de partie
  def playAgainDisplay(): Unit = {
    death.play()
    display.clear(Color.yellow)
    display.drawFancyString(100, 100, s"${if(grid.gameOverA && grid.gameOverB) "Tie" else if(grid.gameOverB){"Player A won"}else if(grid.gameOverA){"Player B won"}}", Color.black, 20)
    display.drawFancyString(100, 200, "Press p to play again or q to quit", Color.black, 20)
  }

  // Fonction qui raffraichit l'affichage à chaque frame
  def updateDisplay(): Unit = {

    display.clear()
    display.setColor(Color.darkGray)
    for (i: Int <- 0 until WIDTH) {
      for (j: Int <- 0 until HEIGHT) {
        display.setPixel(i, j)
      }
    }
    for (i: Int <- 0 until grid.board.length) {
      for (j: Int <- 0 until grid.board(0).length) {
        if (grid.board(i)(j).player == 'a') {
          display.setColor(Color.blue)
          display.drawFillRect(j * (WIDTH / 25), i * (HEIGHT / 25), (j + 1) * (WIDTH / 25), (i + 1) * (HEIGHT / 25))
        }
        if (grid.board(i)(j).player == 'b') {
          display.setColor(Color.red)
          display.drawFillRect(j * (WIDTH / 25), i * (HEIGHT / 25), (j + 1) * (WIDTH / 25), (i + 1) * (HEIGHT / 25))
        }
        if (grid.board(i)(j).player == 'z' && grid.board(i)(j).snakePos == 0) {
          display.setColor(Color.darkGray)
          display.drawFillRect(j * (WIDTH / 25), i * (HEIGHT / 25), (j + 1) * (WIDTH / 25), (i + 1) * (HEIGHT / 25))
        }
        if (grid.board(i)(j).player == 'z' && grid.board(i)(j).snakePos == -1) {
          display.setColor(Color.yellow)
          display.drawFillRect(j * (WIDTH / 25), i * (HEIGHT / 25), (j + 1) * (WIDTH / 25), (i + 1) * (HEIGHT / 25))
        }
        display.setColor(Color.black)
        for (y: Int <- HEIGHT / grid.HEIGHT until HEIGHT by HEIGHT / grid.HEIGHT) {
          display.drawLine(0, y, HEIGHT - 1, y)
        }
        for (x: Int <- WIDTH / grid.WIDTH until WIDTH by WIDTH / grid.WIDTH) {
          display.drawLine(x, 0, x, WIDTH - 1)
        }
      }
    }

  }

  // Appel du keyListener
  display.setKeyManager(k)

  // Fonction qui contient la logique de jeu
  def play(): Unit = {
    waitForInput = false
    playAgain = false
    while (!start) {
      startScreen = true
      display.frontBuffer.synchronized {
        startDisplay()
      }
    }
    startScreen = false
    grid.setGrid()
    grid.spawnPlayer()
    updateDisplay()
    grid.spawnFood()
    grid.spawnFood()
    while (!grid.gameOverA && !grid.gameOverB) {
      grid.movement('a', playerADirection)
      grid.movement('b', playerBDirection)
      display.frontBuffer.synchronized {
        updateDisplay()
      }
      display.syncGameLogic(8)
    }
    display.frontBuffer.synchronized {
      playAgainDisplay()
    }
    start = false
  }
  // Appel de la fonction pour lancer la première partie
  play()
  // Boucle pour pouvoir relancer une partie depuis l'interface utilisateur sans devoir rerun le code
  while (!end) {
    waitForInput = true
    Thread.sleep(100)
    if (playAgain) {
      youpi.play()
      waitForInput = true
      grid.gameOverA = false
      grid.gameOverB = false
      playerADirection = "right"
      playerBDirection = "left"
      play()
    }
  }
  quit.play()
  Thread.sleep(1000)
  System.exit(1)
}
