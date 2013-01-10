import collection.immutable.Seq
import scala.compat.Platform._
import scala.actors._
//import tools.nsc.interpreter.NamedParam
import xml.NodeSeq

//import akka.actor._
//import akka.routing.Round

//import NetInterpreter._

import org.apache.log4j.{Level, Logger, BasicConfigurator}
import org.apache.log4j.PropertyConfigurator



object Floyd3  {

  final val dataDir = "/home/totala/Floyd2/"

   // get a logger instance named "com.foo"
   val logger = Logger.getLogger("Floyd3")

//  logger.setup = Full(Log4j.withFile(getClass().getResource(dataDir + "log4j.xml")))
  PropertyConfigurator.configure(dataDir + "log4j.xml")



   // Now set its level. Normally you do not need to set the
   // level of a logger programmatically. This is usually done
   // in configuration files.
   logger.setLevel(Level.INFO)

//   Logger barlogger = Logger.getLogger("com.foo.Bar");

	// Default locus for this instance
	final val locus = "Earth:USA:California:Sunnyvale:Floyd:Office:work".split(":").toList

	// Technically... the calendar we're using, i.e. Gregorian
	// val Calendar: GregorianCalendar

	// Latitude & Longitude (for modeling Sun)
	final val loc = (37.3490, 122.0263)

	// Some default Synonyms for locus mapping
	val Synonyms = Map("1401 Floyd" -> "Floyd", "CA" -> "California")

	// Start Network interpreter (need the interpreter core for unpersist)
	//NetInterpreter.start

	// Data directory for persistence
	val myScheduler = new FloydScheduler

	// This file (in dataDir) contains all objects that exist
	// i.e. the Objects map?

	var Objects = scala.collection.mutable.Map.empty[String, (OutputChannel[Any], String)]
  final val actorxml = scala.xml.XML.loadFile(dataDir + "actors.xml")

//  def main(args : Array[String]) : Unit = {
  def main(args : Array[String]) {
		logger.info("Starting Floyd at " + currentTime + ", args:")
		logger.info("Floyd3, with ss/sr");
		for (arg <- args)
			logger.info(arg)

    UnpersistObjects
		//HardCodedModels

    val x10Server = x10.start("srv1.otala.net", 3000)
	}

  var t = SunTime(loc._1, loc._2, true)
  logger.info("SunRise = "+ t._1 + ":"+ t._2)
  t = SunTime(loc._1, loc._2, false)
  logger.info("SunSet = "+ t._1 + ":"+ t._2)


  Syslog.start
  DHCPdMonitor.start
  CameraMotion.start
  //XMPPNotifier.start

//   scala.tools.nsc.Interpreter.break(DebugParam("loc", loc))


  def UnpersistObjects() {
    logger.info("Unpersisting objects -- starting: " + actorxml.toString)
    for (o <- actorxml \\ "Event") yield {
        logger.info("\tName: " + (o \ "@id").text)
        logger.info("\t\tClass: " + (o \ "Class").text)
        logger.info("\t\tSchedule: " + (o \ "Schedule").text)
        logger.info("\t\tAction(s): " + (o \ "Action").mkString(","))
//        val obj = new SchedObj((o \ "@id").text, (o \ "Schedule").text);
        val obj = new SchedObj(o);
        obj.start();
        Floyd3.Objects += (o \ "@id").text -> (obj, "test")
      }
    logger.info(Floyd3.Objects)
    logger.info("Unpersisting objects -- complete")

  }

}

import java.util._
import Floyd3.logger

case class SignUpA(time: String, ref: String)
//case class SignUpD(time: String, ref: String)
case class SignUpLong(time: Long, ref: String)
case class TimeEvent(ref: String)
case class Cancel(ref: String)

trait SimpleSchedule extends ReplyReactor {
	// Abstract values
	val schedulestr: String		// Schedule string, e.g. "now + 00:00:02"
	def action: Unit			// Action to be performed when schedule strikes
	// Overhead act() function, calls action()
	def act {
		logger.info("SimpleSchedule object starting: " + schedulestr + ", sched: " + Floyd3.myScheduler)
		Floyd3.myScheduler ! SignUpA(schedulestr, "test2")
		loop {
			react {
				case TimeEvent(ref: String) =>
					action
//					sender ! SignUp(schedulestr, "test2")
			}
		}
	}
}

//import scala.tools.nsc.Interpreter._
//import scala.tools.nsc._
//import Interpreter._
//scala.tools.nsc.interpreter.ILoop

//class SchedObj(n: String, sched: String) extends SimpleSchedule {
class SchedObj(x: scala.xml.NodeSeq) extends SimpleSchedule {
  import Floyd3.logger
//  val schedulestr = sched
  val name = (x \ "@id").text
  val schedulestr = (x \ "Schedule").text
  val Once = (x \ "Class").text.contains("Once")
  var actions =
    for (a <- x \ "Action")
      yield (a.text, (a \ "@when").text)
  val origActions = actions
  logger.debug("A=" + actions)
  def action {
    do {
      var firstAction = actions.head._1
      logger.debug("AI=" + firstAction)
      logger.debug("action, AI=" + firstAction + " @ " + "=>" + x10.x10translateIn(firstAction))
      logger.debug("Once = " + Once)
      if (x10.x10translateIn(firstAction).matches("Unknown") == false)
        x10.x10cmd(x10.x10translateIn(firstAction), firstAction.split(":").tail.head)
      actions = actions.tail
    } while (actions.isEmpty == false && actions.head._2 == "")
    if (actions.isEmpty == false) {
      logger.debug("name="+name+"subtask: " + actions.head._2 + ", actions="+actions)
      Floyd3.myScheduler ! SignUpA(actions.head._2, "subtask")
      //NamedParam("subtask", actions.head._2)
    } else if (Once == false) {
      actions = origActions
      Floyd3.myScheduler ! SignUpA(schedulestr, "main")
    }
  }
}


class FloydScheduler extends Actor {

	import java.util.Calendar._
	import scala.math.{Pi,abs,floor,atan2,sin,round,sqrt,cos}

	def normalize2Pi(x: Double) : Double =
			if (x < 0)
				normalize2Pi(x + 2 * Pi)
			else if (x >= 2 * Pi)
				normalize2Pi(x - 2 * Pi)
			else
				x

	/* For lat/lon, rise (1=SunRise, 0=SunSet), give Hour & Min */
	def SunTime(latitude: Double, longitude: Double, rise: Boolean): (Int, Int) = {
		// FIXME: Should pass the target date, as opposed to using today's date
		val TimeZone : Int = abs(Calendar.getInstance.getTimeZone.getRawOffset +
							Calendar.getInstance.get(Calendar.DST_OFFSET)) / 1000 / 3600
		val E : Double = 0.0174533 * latitude
		val F : Double = 0.0174533 * longitude
		val J : Double = if (rise) 1.5 * Pi else Pi / 2
		val K : Double = Calendar.getInstance.get(Calendar.DAY_OF_YEAR) + ((J + F) / (2 * Pi))
		val L : Double = ((K * 0.017202) - 0.0574039)
		val M : Double = normalize2Pi(L + 0.0334405 * sin(L) + 4.93289 + (3.49066E-04) * sin(2 * L))
		var P : Double = atan2(0.91746 * (sin(M) / cos(M)), 1)
		if (M > 1.5 * Pi) P += 2 * Pi else if (M > Pi / 2) P += Pi
		var Q : Double = 0.39782 * sin(M)
		Q = atan2(Q / sqrt(-Q * Q + 1), 1)
		var S : Double = (-0.0145439 - (sin(Q) * sin(E))) / (cos(Q) * cos(E))
		if (abs(S) > 0) (0, 0)
		S = Pi / 2 - atan2(S / sqrt(-S * S + 1), 1)
		if (rise) S = 2 * Pi - S
		val T : Double = S + P - 0.0172028 * K - 1.73364
		val V : Double = normalize2Pi(T + F - 0.261799 * TimeZone) * 3.81972
		(floor(V).toInt, round((V - floor(V)) * 60).toInt)
	}
  import Floyd3.loc

  final val hhmmss_r = """(\d\d):(\d\d):?(\d\d)?""".r
//  val Mult_r = """(\d+)(y|year|mth|w|week|d|day|h|hour|m|s)""".r
// val Mult_r = """(\+|\-)(\d+)(w|d|h|m|s)""".r
  final val Mult_r = """(\d+)(w|d|h|m|s)""".r

  final val MultMap = collection.immutable.Map("y" -> YEAR,
                                        "year" -> YEAR,
                                        "mth" -> MONTH,
                                        "w" -> WEEK_OF_YEAR,
                                        "week" -> WEEK_OF_YEAR,
                                        "d" -> DAY_OF_YEAR,
                                        "day" -> DAY_OF_YEAR,
                                        "h" -> HOUR_OF_DAY,
                                        "hour" -> HOUR_OF_DAY,
                                        "m" -> MINUTE,
                                        "s" -> SECOND)

  final val WeekdayNamesS = "sun,mon,tue,wed,thu,fri,sat".split(",").toList
  final val WeekdayNamesS_r = """(sun|mon|tue|wed|thu|fri|sat)[a-z]*""".r
  final val MonthNamesS = "jan,feb,mar,apr,may,jun,jul,aug,sep,oct,nov,dec".split(",").toList
  final val MonthNamesS_r = """(jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec)[a-z]*""".r
//  val yyyy_r = """(20\d\d)""".r
//  val date_r = """(\d+)""".r
//  val Weekday = "weekday"
//  val Weekend = "weekend"

  import scala.collection.immutable.List
  import java.util.Calendar._
  import Floyd3.logger

  def patternMatchDue(p: String) : Long = patternMatchDueBase(p, new GregorianCalendar)

  def patternMatchDueBase(p: String, Due: GregorianCalendar) : Long = {
    var Times = List[Int]()
    var Years = List[Int]()
    var Weekdays = List[Int]()
    var Months = List[Int]()
    var Dates = List[Int]()

    logger.debug("patternMatch with: " + p.toLowerCase.split(" ,"))

    var sign = 1

    for (i <- p.split(' ')) {

      logger.debug("SubPattern = >" + i + "<")

      i.toLowerCase match {
        case "+" =>
          sign = 1
        case "-" =>
          sign = -1
        case Mult_r(num, mult) => {
//          Due.add(MultMap.get(mult).get, (if (sign == "+") 1 else -1) * num.toInt)
          Due.add(MultMap.get(mult).get, sign * num.toInt)
          Times = (Due.get(HOUR_OF_DAY) * 3600 + Due.get(MINUTE) * 60 + Due.get(SECOND)) :: Times
          logger.debug("mult_r: num=>" + num + "< mult=>"+mult+"<")
        }
        case hhmmss_r(hh, mm, ss) =>
          Times = (hh.toInt * 3600 + mm.toInt * 60 + (if (ss != null) ss.toInt else 0)) :: Times
        case "sunset" =>
          // FIXME: Should pass the target date, instead of using today's date
          val t = SunTime(loc._1, loc._2, false)
          Times = (t._1 * 3600 + t._2 * 60) :: Times
        case "sunrise" =>
          // FIXME: Should pass the target date, instead of using today's date
          val t = SunTime(loc._1, loc._2, true)
          Times = (t._1 * 3600 + t._2 * 60) :: Times
        case WeekdayNamesS_r(day) =>
          Weekdays = WeekdayNamesS.indexOf(day) + 1 :: Weekdays
        case MonthNamesS_r(mth) =>
          Months = MonthNamesS.indexOf(mth) :: Months
//        case yyyy_r(year) =>
//          if (year.toInt >= Due.get(YEAR))
//            Years = year.toInt :: Years
//        case date_r(date) =>
//          Dates = date.toInt :: Dates
        case "weekday" =>
          Weekdays = MONDAY :: TUESDAY :: WEDNESDAY :: THURSDAY :: FRIDAY :: Weekdays
        case "weekend" =>
          Weekdays = SATURDAY :: SUNDAY :: Weekdays
        case _ =>
          logger.warn("Didn't understand: >" + i + "<")
      }
    }

    logger.info("Times: " + Times)
    var TimeOfDay = Due.get(HOUR_OF_DAY) * 3600 + Due.get(MINUTE) * 60 + Due.get(SECOND)
    if (Times.exists(_ >= TimeOfDay))
      TimeOfDay = Times.filter(_ >= TimeOfDay).min
    else {
      Due.add(DAY_OF_YEAR, 1)
      logger.info("Times: " + Times)
      logger.info("Times.min: " + Times.min)
      TimeOfDay = Times.min
    }
    Due.set(HOUR_OF_DAY, TimeOfDay / 3600)
    Due.set(MINUTE, (TimeOfDay % 3600) / 60)
    Due.set(SECOND, ((TimeOfDay % 3600) % 60))
    Due.set(MILLISECOND, 0)

    // Are years defined?
    if (Years.isEmpty == false)
      // Is current year listed?
      if (Years.contains(Due.get(YEAR)) == false)
        Due.set(YEAR, Years.min)	// Set to smallest value (next)

    // Are months defined?
    if (Months.isEmpty == false)
      // Is current month listed?
      if (Months.contains(Due.get(MONTH)) == false)
        // Any more months this year?
        if (Months.exists(_ > Due.get(MONTH)) == false) {
          if (Years.isEmpty == false)
            Due.set(YEAR, Years.filter(_ > Due.get(YEAR)).min)
          else
            Due.add(YEAR, 1)	// Go to next year
          Due.set(MONTH, Months.min)		// Pick first month
        } else
          // Pick the earliest month, after this month
          Due.set(MONTH, Months.filter(_ > Due.get(MONTH)).min)

    // Is Monthday defined?
    val MonthDay = Due.get(DAY_OF_MONTH)
    if (Dates.isEmpty == false)
      // Is current date listed?
      if (Dates.contains(MonthDay) == false)
        if (Dates.exists(_ > MonthDay))
          Due.set(DAY_OF_MONTH, Dates.filter(_ > MonthDay).min)
        else {
          Due.set(DAY_OF_MONTH, Dates.min)
          Due.add(MONTH, 1)		// FIXME: Add based on Months, if defined
        }

    // Are weekdays defined?
    val Today = Due.get(DAY_OF_WEEK)
    if (Weekdays.isEmpty == false)
      // Is today listed?
      if (Weekdays.contains(Today) == false)
        if (Weekdays.exists(_ > Today))
          Due.add(DAY_OF_YEAR, Weekdays.filter(_ > Today).min - Today)
        else
          Due.add(DAY_OF_YEAR, 7 + Weekdays.min - Today)

    logger.info("Due date: " + Due.getTime)
    Due.getTimeInMillis

  }

	def makeUnique(time: Long): Long =
		if (Queue.contains(time)) makeUnique(time + 1) else time

	def minTimeout(x: Long): Long = if (x < 100) 100 else x

	def nextEvent: Long = if (Queue.isEmpty) 10000 else minTimeout(Queue.firstKey - currentTime)

	// Queue of Scheduled events to notify
	var Queue = scala.collection.immutable.SortedMap.empty[Long, (OutputChannel[Any], String)]

	def act {
		logger.info("Scheduler starting")
    patternMatchDue("weekday 08:00")
		var timeout = nextEvent
		while (true) {
			receiveWithin(timeout) {
        case SignUpA(time: String, ref: String) =>
          Queue += makeUnique(patternMatchDue(time)) -> (sender, ref)
				case SignUpLong(time: Long, ref: String) =>
					Queue += makeUnique(time) -> (sender, ref)
					logger.info("SignUp/TimerQueue: " + Queue)
				case TimeEvent(ref: String) =>
					logger.info("TimEvent: ref=" + ref)
				case Cancel(ref: String) =>
					logger.warn("Received cancel for " + ref)
				case TIMEOUT =>
					while (Queue.isEmpty == false && Queue.firstKey < currentTime) {
						/* Queue(Queue.firstKey) ! Ping */
						Queue(Queue.firstKey)._1 ! TimeEvent(Queue(Queue.firstKey)._2)
						logger.debug("Dispatching: " + Queue)
						Queue = Queue.tail
						logger.debug("Removing from queue: " + Queue)
					}
			}
			timeout = nextEvent
		}
		logger.warn("Scheduler exiting")
	}

	start
}

