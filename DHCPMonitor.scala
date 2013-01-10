//package

/**
 * Created by IntelliJ IDEA.
 * User: totala
 * Date: 12/19/11
 * Time: 9:20 AM
 * To change this template use File | Settings | File Templates.
 */

import Floyd3._
import java.io._
//import java.net.{InetAddress,ServerSocket,Socket,SocketException}
import java.net._
import java.util._

import scala.concurrent.ops._

import scala.actors._
import scala.actors.Actor._
import scala.io._

import scala.compat.Platform._
import org.apache.log4j.{Level, Logger, BasicConfigurator}
import Floyd3.logger


/* Make MAC map
 * 5dd
 * %s/ /">/
 * %s/^/<MAC id="/
 * %s/$/<\/MAC>/
 * %s/&/+/g
 * Add <MACs> on line 1
 * Add </MACs> on last line
 */


// To get UDP/514 traffic into UDP/1514...
//  sudo iptables -t nat -I PREROUTING -p udp --dport 514 -j REDIRECT --to-ports 1514


case class SyslogSubscribe(app: String, actor: Actor)
case class SyslogMsg(msg: String)

// Interesting facts:
// 		Identifier (MAC or IP)
//		LastSeen? FirstTimeSeen? Pattern?
object DHCPdMonitor extends Actor {
	// MAC -> IP, hostname
	val Machines = new scala.collection.mutable.HashMap[String, InetAddress]
	// MAC -> Vendor
	val MACs = scala.xml.XML.loadFile(Floyd3.dataDir + "nmap-mac-prefixes")

  Syslog ! SyslogSubscribe("dhcpd", this)

	def MACvendor(mac: String): String = {
		// partialMAC: 00:21:06:40:3e:59 -> 002106
		val partialMAC = mac.replaceAll(":","").substring(0,6).toUpperCase
		// get list of matching vendors
		val vendor = for (i <- MACs \\ "MAC" if (i \ "@id").text == partialMAC) yield i.text
		if (vendor.isEmpty) "Unknown" else vendor.first
	}

	// Register interest with app=dhcpd, pattern="DHCPACK on IP to MAC"
	def act {
		Syslog ! SyslogSubscribe("dhcpd", this)
		while (true) {
			receive {
				case SyslogMsg(msg: String) =>
//					println("DHCPD message received: " + msg)
					// DHCPDISCOVER from 00:21:06:40:3e:59 via eth0
					// DHCPOFFER on 192.168.254.55 to 00:21:06:40:3e:59 via eth0
					// DHCPREQUEST for 192.168.254.55 (192.168.254.1) from 00:21:06:40:3e:59 via eth0
					// DHCPACK on 192.168.254.55 to 00:21:06:40:3e:59 via eth0
					// 0       1  2              3  4                 5   6
					// DHCPRELEASE of 192.168.254.56 from 00:22:4c:01:83:1f via eth0 (not found)
					// DHCPINFORM from 192.168.254.54 via eth0: not authoritative for subnet 192.168.254.0
					if (msg.startsWith("DHCPACK")) {
						val dhcpAck = msg split ' '
						if (Machines.contains(dhcpAck(4)) == false) {
							val name = InetAddress.getByName(dhcpAck(2)).getHostName
							Machines += dhcpAck(4) -> InetAddress.getByName(dhcpAck(2))	// MAC => IP
							logger.info("New host " + dhcpAck(4) + " (" + MACvendor(dhcpAck(4)) + ") seen")
							logger.info("Machine Map updated: " + Machines)

             // for (i <- Machines)


						/*
						Machines foreach {
							(kv) => println("MAC=" + kv._1 + ", IP=" + kv._2 + ", Host=" + kv._2.getHostName())
						}
						*/
            }
					}
			}
		}
	}
}

object CameraMotion extends Actor {
	val Motion = new scala.collection.mutable.HashMap[String, Long]
	// Register interest with app=vsftpd; check that message contains "UPLOAD" and "[camera]" -- then extract the name of the motion sensitive camera from the path
  Syslog ! SyslogSubscribe("vsftpd", this)
	def act {
		while (true) {
			receive {
				case SyslogMsg(msg: String) =>
					// Msg=Tue Aug  4 14:59:28 2009 [pid 20592] [camera] OK LOGIN: Client "192.168.254.70"
					// Msg=Tue Aug  4 14:59:28 2009 [pid 20594] [camera] OK UPLOAD: Client "192.168.254.70", "/home/camera/livingroom/20090804/145929_2.jpg", 37577 bytes, 1613.24Kbyte/sec
					//     0                                                                                 1                                                2            3
					if (msg.contains("UPLOAD") && msg.contains("[camera]")) {
						val upload = msg split ','
						val camname = upload(1) split '/'
						Motion += camname(3) -> currentTime
						logger.info("Upload: " + camname(3))
					}
			}
		}
	}
}


object Syslog extends Actor {

	val DEFAULT_PORT = 514

	val PRIORITY_USER = 8
	val PRIORITY_DAEMON = 24
	val PRIORITY_LOCAL0 = 128
	val PRIORITY_LOCAL1 = 136
	val PRIORITY_LOCAL2 = 144
	val PRIORITY_LOCAL3 = 152
	val PRIORITY_LOCAL4 = 160
	val PRIORITY_LOCAL5 = 168
	val PRIORITY_LOCAL6 = 176
	val PRIORITY_LOCAL7 = 184

	private val SEVERITY_EMERGENCY = 0
	private val SEVERITY_ALERT = 1
	private val SEVERITY_CRITICAL = 2
	private val SEVERITY_ERROR = 3
	private val SEVERITY_WARNING = 4
	private val SEVERITY_NOTICE = 5
	private val SEVERITY_INFO = 6
	private val SEVERITY_DEBUG = 7

	var Interests = scala.collection.immutable.HashMap[String, Actor]()

	def act {
		loop {
			react {
				case SyslogSubscribe(app: String, actor: Actor) =>
					Interests += app -> actor
          logger.info("Interests: "+ Interests)
			}
		}
	}

//	def trimws(str: String): String = if (str.endsWith(" ") || str.endsWith("\r") || str.endsWith("\n")) trimws(str.substring(0, str.length - 1)) else str
	def trimws(str: String): String = if (str.endsWith(" ") || str.contains("\r") || str.contains("\n")) trimws(str.substring(0, str.length - 1)) else str

//	def receiverLoop {
		var Ignored = 0
		var DontDisplay = 0
		val socket = new DatagramSocket(1514)	// Going for non-privileged port, 1000+514
		final val maxLen = 2048
		logger.info("Syslog receiver started")
		spawn {
			val buff = new Array[Byte](maxLen)
			var packet = new DatagramPacket(buff, maxLen)
			val lineRegex = """(<\d+>)([a-zA-Z0-9_\(\)\-\.]+)(\[\d+\])?:\s*(.+)""".r
			//                  pri     app       pid         msg
			while (true) {
				socket.receive(packet)
				val str = UTF8Codec.decode(packet.getData, 0, packet.getLength).stripLineEnd
//				println("Before Regex: '" + str + "'")
				try {
					val lineRegex(pri, app, pid, msg) = trimws(str)
//					println("Regex: pri=" + pri + ", app=" + app + ", pid=" + pid + ", msg=" + msg)
					if (app.startsWith("HomeDaemon") || app == "sendmail" || app == "imapd" || app == "ipop3d" || app == "crond" || app == "spamd" ||
							app == "xinetd" || app == "kernel" || app == "dhcpd" || app == "vsftpd")
						DontDisplay += 1
					else if (msg.contains("lame server resolving") == false)
						logger.info("Addr=" + packet.getAddress + ":" + packet.getPort + ", App=" + app + ", Msg=" + msg)

					Interests.get(app) match {
						case Some(interested) =>
							interested ! SyslogMsg(msg)
						case None =>
							Ignored += 1
					}
				} catch {
					case _ =>
						if (str.contains("last message repeated") == false)
							logger.warn("Bad Syslog input line: " + str)
				}
			}
		}
//	}
}
