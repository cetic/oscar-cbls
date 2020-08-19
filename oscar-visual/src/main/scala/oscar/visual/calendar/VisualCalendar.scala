package oscar.visual.calendar

import oscar.visual.VisualDrawing
import java.util.GregorianCalendar
import java.util.Calendar

import oscar.visual.VisualFrame
import javax.swing.SwingUtilities
import java.util.Locale

import oscar.visual.shapes.VisualText
import java.awt.{Color, Font, FontMetrics}

class VisualCalendar(startDate: GregorianCalendar, nMonthsToDisplay: Int) extends VisualDrawing(false, false) {
  assert(nMonthsToDisplay > 0)
  val X_OFFSET = 50
  val Y_OFFSET = 50
  val CAL_TILE_SIZE = 50
  val SPACE_BETWEEN_TILES = 5
  val SPACE_BETWEEN_MONTHS = 125
  
  val monthWidth: Int = 7 * CAL_TILE_SIZE + 6 * SPACE_BETWEEN_TILES
  val fm: FontMetrics = getFontMetrics(getFont)
  val fontHeight: Int = fm.getHeight
  val weekOfYearWidth: Int = fm.stringWidth(" 01 ")
  
  val firstDate: GregorianCalendar = startDate.clone().asInstanceOf[GregorianCalendar]
  firstDate.add(Calendar.DAY_OF_MONTH, 1 - firstDate.get(Calendar.DAY_OF_MONTH))
  
  val MONTH_NAMES: Array[String] = Array("JANUARY", "FEBRUARY", "MARCH", "APRIL", "MAY", "JUNE",
      "JULY", "AUGUST", "SEPTEMBER", "OCTOBER", "NOVEMBER", "DECEMBER")
      
  val WEEK_DAY_NAMES: Array[String] = Array("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
  
  def getTotalNDays(month: Int): Int = {
    val curDate = startDate.clone().asInstanceOf[GregorianCalendar]
    curDate.add(Calendar.MONTH, month)
    curDate.getActualMaximum(Calendar.DAY_OF_MONTH)
  }
  
  def getCorrectDayOfWeek(day: Int): Int = {
    if (day - 2 == -1) 6
    else day - 2
  }
  
  val datesToDisplay: Array[Array[GregorianCalendar]] = Array.tabulate(nMonthsToDisplay)(i => {
    Array.tabulate(getTotalNDays(i))(j => {
      val newDate = firstDate.clone().asInstanceOf[GregorianCalendar]
      newDate.add(Calendar.MONTH, i)
      newDate.add(Calendar.DAY_OF_MONTH, j)
      newDate.setFirstDayOfWeek(Calendar.MONDAY)
      newDate.setMinimalDaysInFirstWeek(1)
      newDate
    })
  })
  
  val monthNames: Array[String] = datesToDisplay.map(month => MONTH_NAMES(month(0).get(Calendar.MONTH)))
  
  val monthNameTexts: Array[VisualText] = Array.tabulate(monthNames.length)(i => {
    new VisualText(this,
        X_OFFSET + weekOfYearWidth + monthWidth / 2 + i * (SPACE_BETWEEN_MONTHS + monthWidth),
        Y_OFFSET,
        monthNames(i),
        centered=true)
  })
  
  for (monthNameTxt <- monthNameTexts) {
    monthNameTxt.setFont(new Font(Font.SANS_SERIF, Font.BOLD, 16))
  }
  
  val dayNameTexts: Array[Array[VisualText]] = Array.tabulate(monthNames.length)(i => {
    Array.tabulate(WEEK_DAY_NAMES.length)(j => {
      new VisualText(this,
        X_OFFSET + weekOfYearWidth + (CAL_TILE_SIZE / 2) + j * (CAL_TILE_SIZE + SPACE_BETWEEN_TILES) + i * (SPACE_BETWEEN_MONTHS + monthWidth),
        Y_OFFSET + 2 * fontHeight,
        WEEK_DAY_NAMES(j),
        centered=true)
    })
  })
  
  for (i <- dayNameTexts.indices; j <- dayNameTexts(i).indices) {
    if (j == 6) dayNameTexts(i)(j).fontColor = Color.RED
  }
  
  val weekNumberTexts: Array[Array[VisualText]] = Array.tabulate(datesToDisplay.length)(i => {
    Array.tabulate(datesToDisplay(i)(0).getActualMaximum(Calendar.WEEK_OF_MONTH))(j => {
      new VisualText(this,
        X_OFFSET + i * (SPACE_BETWEEN_MONTHS + monthWidth),
        Y_OFFSET + 3 * fontHeight + (CAL_TILE_SIZE / 2) + j * (CAL_TILE_SIZE + SPACE_BETWEEN_TILES),
        "%02d" format datesToDisplay(i)(math.min(j * 7, datesToDisplay(i)(0).getActualMaximum(Calendar.DAY_OF_MONTH) - 1)).get(Calendar.WEEK_OF_YEAR),
        centered=true)
    })
  })
      
  val daysRectangle: Array[Array[VisualCalendarTile]] = Array.tabulate(nMonthsToDisplay)(i => {
    Array.tabulate(datesToDisplay(i).length)(j => {
      new VisualCalendarTile(this,
         X_OFFSET + weekOfYearWidth + (CAL_TILE_SIZE + SPACE_BETWEEN_TILES) * getCorrectDayOfWeek(datesToDisplay(i)(j).get(Calendar.DAY_OF_WEEK)) + i * (SPACE_BETWEEN_MONTHS + monthWidth),
         Y_OFFSET + 2.5 * fontHeight + (CAL_TILE_SIZE + SPACE_BETWEEN_TILES) * (datesToDisplay(i)(j).get(Calendar.WEEK_OF_MONTH) - 1),
         CAL_TILE_SIZE,
         "%02d" format datesToDisplay(i)(j).get(Calendar.DAY_OF_MONTH), 10, 7)
    })
  })
  
  def colorDay(col: Color, date: GregorianCalendar): Unit = {
    val monthIndex = getMonthDifference(firstDate, date)
    colorDay(col, monthIndex, date.get(Calendar.DAY_OF_MONTH) - 1)
  }
  
  def colorDay(col: Color, monthIndex: Int, dayIndex: Int): Unit = {
    daysRectangle(monthIndex)(dayIndex).innerCol = col
  }
  
  def update(): Unit = {
    SwingUtilities.invokeLater(() => {
      repaint()
    })
  }
  
  // Returns the number of days between lastDate and firstDate
  def getMonthDifference(firstDate: GregorianCalendar, lastDate: GregorianCalendar): Int = {
    val diffYear = lastDate.get(Calendar.YEAR) - firstDate.get(Calendar.YEAR)
    diffYear * 12 + lastDate.get(Calendar.MONTH) - firstDate.get(Calendar.MONTH)
  }
}

object VisualCalendar {
  def apply(startDate: GregorianCalendar, nMonthsToDisplay: Int) = new VisualCalendar(startDate, nMonthsToDisplay)
  
  def main(args : Array[String]): Unit = {
	val f = VisualFrame("toto")
	val inf = f.createFrame("Drawing")
	
	val visualCalendar = VisualCalendar(new GregorianCalendar(Locale.FRANCE), 3)
	
	inf.add(visualCalendar)
	f.pack()
	
	val inThreeDays = new GregorianCalendar(Locale.FRANCE)
	inThreeDays.add(Calendar.DAY_OF_YEAR, 3)
	visualCalendar.colorDay(Color.GREEN, inThreeDays)
	visualCalendar.repaint()
  }
}