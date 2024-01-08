import hevs.graphics.FunGraphics
import hevs.graphics.utils.GraphicsBitmap

import java.awt.Color
import java.awt.event.{KeyEvent, KeyListener}

object Runner extends App {
  val WIDTH: Int = 500
  val HEIGHT: Int = 500
  var gameOver: Boolean = false
  var display: FunGraphics = new FunGraphics(WIDTH, HEIGHT)
  var playerADirection: String = "right"
  var playerBDirection: String = "left"
  var k: KeyListener = new KeyListener {
    override def keyTyped(e: KeyEvent): Unit = {

    }

    override def keyPressed(e: KeyEvent): Unit = {

    }
    override def keyReleased(e: KeyEvent): Unit = {
      e.getKeyCode match {
        case KeyEvent.VK_UP => playerBDirection = "up"
        case KeyEvent.VK_DOWN => playerBDirection= "down"
        case KeyEvent.VK_LEFT => playerBDirection = "left"
        case KeyEvent.VK_RIGHT => playerBDirection = "right"
        case KeyEvent.VK_W => playerADirection = "up"
        case KeyEvent.VK_S => playerADirection = "down"
        case KeyEvent.VK_D => playerADirection = "right"
        case KeyEvent.VK_A => playerADirection = "left"
        case _ => {
        }
      }
    }
  }
  var grid: Board = new Board()

  // display.displayFPS(true)

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

  grid.spawnPlayer()
  updateDisplay()
  display.setKeyManager(k)
  grid.spawnFood()
  while (!grid.gameOverA && !grid.gameOverB) {
    grid.movement('a', playerADirection)
    grid.movement('b', playerBDirection)
    display.frontBuffer.synchronized{

      updateDisplay()
    }
    display.syncGameLogic(8)
  }
  if (grid.gameOverA && grid.gameOverB) println("Tie")
  else if(grid.gameOverB) println("A won")
  else if (grid.gameOverA) println("B won")
}
