// package

import java.io._
import java.net.{InetAddress,ServerSocket,Socket,SocketException}

import concurrent.ops._

import scala.actors._
import scala.actors.Actor._
import org.apache.log4j.{Level, Logger, BasicConfigurator}
import Floyd3.logger
import scala.concurrent



/**
 * Created by IntelliJ IDEA.
 * User: totala
 * Date: 12/18/11
 * Time: 6:00 PM
 * To change this template use File | Settings | File Templates.
 */

case class Send(command: String)

object x10 {

    var out: DataOutputStream = null
    var in: BufferedReader = null
    var socket: Socket = null
    var ack = false

    def x10cmd(cmd: String,  act: String) {
      x10Send(cmd + cmd.head + " " + act + "\n")
    }
    def x10Send(command: String) {
      synchronized {
        logger.debug("x10send " + command)
        ack = false
        out.writeBytes(command)
        out.flush
      }
    }

    def action(dev: String, state: String) {
      if (dev.contains('='))
        action(dev.substring(0,dev.indexOf('=')), dev.substring(dev.indexOf('='))+1)
      else
        // For whatever reason, x10net requires C08C, i.e. house-code + unit-number + house-code
        x10Send(x10translateIn(dev) + x10translateIn(dev)(0) + state + '\n')
    }

    def start(host: String, port: Int): Boolean = {
      logger.info("Starting X10 receiver")
      try {
        val ia = InetAddress.getByName(host)
        socket = new Socket(ia, port)
        out = new DataOutputStream(socket.getOutputStream())
        in = new BufferedReader(new InputStreamReader(socket.getInputStream()))
      } catch {
        case e: IOException =>
          logger.error("x10 could not connect to " + host + ":" + port);
          System.exit(-1)
      }
      /* Testing
      x10Send("C08COFF\n")
      */

      spawn {
        var unit: String = "Unknown"
        while (true) {
          val x = in.readLine()
          /* println("Received " + x) */
          if (x.startsWith("A"))
            ack = true
          /*  println("Acknowledge") */
          else if (x.startsWith("R")) {
            if (x.endsWith("Off"))
              logger.debug(new java.util.Date + ": " + translateOut(unit) + " -> Off")
            else if (x.endsWith("On"))
              logger.debug(new java.util.Date + ": " + translateOut(unit) + " -> On")
            else
              unit = x.substring(2,5)
          } else if (x.startsWith("E"))
            logger.warn("Error")
          else
            logger.warn("Unknown x10 response: " + x)
        }
        out.close()
        in.close()
        socket.close()
      }
      logger.info("X10 receiver started")
      true
    }

    val x10map = scala.xml.XML.loadFile(Floyd3.dataDir + "x10map.xml")

    // Context for all X10 devices -- they are within "Floyd"
    val where = List("Floyd", "Sunnyvale", "California", "USA", "Earth")

    def translateOut(x: String): String = {
      if (x.contains(":")) return translateOut(x.split(":").head)
      val res = for ( val d <- x10map \\ "entry" ; x == (d \ "device").text ) yield
         (d \ "location").text + ":" + (d \ "name").text
      if (res.size == 0) "Unknown(" + x + ")" else res(0)
    }

    def x10translateIn(x: String): String = {
      if (x.contains(":")) return x10translateIn(x.split(":").head)
      logger.debug("x10translateIn " + x)
      val res = for (val d <- x10map \\ "entry" ; x == (d \ "name").text ) yield
           (d \ "device").text
      if (res.size == 0) "Unknown" else res(0)
    }


}