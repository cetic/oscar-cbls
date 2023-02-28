package oscar.cbls.visual.profiling

import oscar.cbls.core.search.profiling.{CombinatorProfiler, Profiler}

import java.awt.event.{MouseAdapter, MouseEvent}
import java.awt.{BorderLayout, Color, Component, Font, GridBagConstraints, GridBagLayout}
import javax.swing.{BorderFactory, JLabel, JPanel, JScrollPane, JTable, JTextArea, SwingConstants, SwingUtilities}
import javax.swing.table.{DefaultTableCellRenderer, DefaultTableModel}

object ProfilingTable {
  def apply(rootProfiler: Profiler): ProfilingTable ={
    val headers = rootProfiler.collectThisProfileHeader
    val goodValueIndicator = rootProfiler.goodValueIndicator()
    new ProfilingTable(rootProfiler, headers, goodValueIndicator)
  }
}

class ProfilingTable(rootProfiler: Profiler, headers: Array[String], goodValueIndicator: Array[Option[String]]) extends JPanel(new GridBagLayout()){
  private val gsm: GeneralStatisticProfilingTableModel = GeneralStatisticProfilingTableModel(rootProfiler, headers)
  private val generalStatisticsTable: JTable = new JTable(gsm){
    override def getScrollableTracksViewportWidth: Boolean ={
      getPreferredSize.width < getParent.getWidth
    }
    override def getScrollableTracksViewportHeight: Boolean ={
      getPreferredSize.height < getParent.getHeight
    }
  }

  private val combinatorStatisticsPanel: JPanel = new JPanel(new BorderLayout())

  generalStatisticsTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS)

  private final val combinatorColor: Color = new Color(0,0,255,80)
  private final val neighborhoodColor: Color = new Color(0,0,0,50)

  generalStatisticsTable.setDefaultRenderer(classOf[Object], new DefaultTableCellRenderer() {
    override def getTableCellRendererComponent(table: JTable, value: Object, isSelected: Boolean, hasFocus: Boolean, row: Int, column: Int): Component = {
      val c = super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column)
      if (column > gsm.commonStatisticsStartAfter) {
        val rank = getStatEffectiveness(column-gsm.commonStatisticsStartAfter)(row)
        c.setBackground(cellColor(rank))
      } else {
        if(gsm.displayedNodes(row).hasChildren) c.setBackground(combinatorColor)
        else c.setBackground(neighborhoodColor)
      }
      c.setFont(
        if(isSelected) new Font(Font.MONOSPACED, Font.BOLD, c.getFont.getSize)
        else new Font(Font.MONOSPACED, Font.PLAIN, c.getFont.getSize))
      c
    }
    override def getHorizontalAlignment: Int = SwingConstants.LEFT
  })

  generalStatisticsTable.addMouseListener(new MouseAdapter {
    override def mouseClicked(e: MouseEvent): Unit = {
      val row = generalStatisticsTable.rowAtPoint(e.getPoint)
      val column = generalStatisticsTable.columnAtPoint(e.getPoint)
      if (row >= 0 && column <= gsm.commonStatisticsStartAfter) {
        if (SwingUtilities.isLeftMouseButton(e) && e.getClickCount == 2)
          gsm.expandAllUnder(row)
        else if (SwingUtilities.isLeftMouseButton(e) && e.getClickCount == 1)
          gsm.expand(row)
        else if (SwingUtilities.isRightMouseButton(e))
          gsm.collapse(row)
        displayCombinatorStatistics(gsm.displayedNodes(row))
      }
    }
  })

  private def getStatEffectiveness(statId: Int): Array[Float] = {
    val displayedNodesValues = gsm.displayedNodes.map(_.profilerCommonData)
    val allValues = displayedNodesValues.map(dnv => statToFloat(dnv.apply(statId)))
    val allUsableValues = allValues.filterNot(_ == Float.NegativeInfinity)
    val worstAndBestValues: Option[(Float, Float)] =
      if(allUsableValues.isEmpty) None
      else goodValueIndicator(statId) match {
        case None => None
        case Some("Max") => Some((allUsableValues.min, allUsableValues.max))
        case Some("Min") => Some((allUsableValues.max, allUsableValues.min))
        case _ => None
      }
    worstAndBestValues match {
      case Some(worstAndBest) =>
        val range = worstAndBest._2 - worstAndBest._1
        allValues.map(statValue =>
          if (range == 0.0f) 1.0f
          else if (statValue == Float.NegativeInfinity) Float.NegativeInfinity
          else (statValue - worstAndBest._1) / range
        ).toArray
      case None =>
        Array.fill(displayedNodesValues.length)(Float.NegativeInfinity)
    }
  }

  private def cellColor(bestWorstRatio: Float): Color ={
    if(bestWorstRatio == Float.NegativeInfinity) return Color.white
    val greenHSB = Color.RGBtoHSB(Color.GREEN.getRed, Color.GREEN.getGreen, Color.GREEN.getBlue,null).head
    val redHSB = Color.RGBtoHSB(Color.RED.getRed, Color.RED.getGreen, Color.RED.getBlue,null).head
    val colorHSB = redHSB + bestWorstRatio*(greenHSB-redHSB)
    val color = Color.getHSBColor(colorHSB,1,1)
    new Color(color.getRed, color.getGreen, color.getBlue, 150)
  }

  private def displayCombinatorStatistics(node: ProfilingTableNode): Unit ={
    combinatorStatisticsPanel.removeAll()

    node match{
      case bn: ProfilingTableBranchNode =>
        bn.profiler match{
          case cp: CombinatorProfiler =>
            val profiledCombinatorData = cp.collectCombinatorSpecificStatistics.filter(_.nonEmpty)
            val title = new JLabel(profiledCombinatorData.head.head)
            title.setFont(new Font(Font.MONOSPACED, Font.BOLD, title.getFont.getSize))
            combinatorStatisticsPanel.add(title, BorderLayout.NORTH)
            val newCombinatorText = new JTextArea(profiledCombinatorData.tail.map(_.mkString("\n")).mkString("\n\n"))
            newCombinatorText.setEditable(false)
            newCombinatorText.setFont(new Font(Font.MONOSPACED, Font.PLAIN, newCombinatorText.getFont.getSize))
            combinatorStatisticsPanel.add(newCombinatorText, BorderLayout.CENTER)
            combinatorStatisticsPanel.revalidate()
        }
      case _ =>
    }
  }

  private def statToFloat(stat: String): Float = {
    try {
      stat.toFloat
    } catch {
      case _: NumberFormatException => Float.NegativeInfinity
    }
  }

  gsm.expandAll()
  gsm.updateData()

  // Add panes into panel

  private val combinatorStatisticsSP: JScrollPane = new JScrollPane(combinatorStatisticsPanel)
  combinatorStatisticsSP.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10))
  combinatorStatisticsSP.setBackground(combinatorColor)

  private val gbc: GridBagConstraints = new GridBagConstraints
  gbc.anchor = GridBagConstraints.FIRST_LINE_START
  gbc.fill = GridBagConstraints.BOTH
  gbc.weightx = 1.0
  gbc.weighty = 0.8
  gbc.gridx = 0
  gbc.gridy = 0
  this.add(new JScrollPane(generalStatisticsTable), gbc)

  gbc.weighty = 0.2
  gbc.gridy = 1
  this.add(combinatorStatisticsSP, gbc)
}

case class GeneralStatisticProfilingTableModel(rootProfiler: Profiler, headers: Array[String]) extends DefaultTableModel {
  private val profilingTableNodes: Array[ProfilingTableNode] = ProfilingTableNode(rootProfiler)
  private var rowToProfilingTableNode: Array[ProfilingTableNode] = Array(profilingTableNodes.head)
  val commonStatisticsStartAfter: Int = 0

  override def isCellEditable(row: Int, column: Int): Boolean = false

  def displayedNodes: List[ProfilingTableNode] = rowToProfilingTableNode.toList

  private def setHeaders(): Unit = headers.foreach(h => this.addColumn(h.toCharArray.mkString("\n")))

  def expand(row: Int): Unit ={
    val ptn = rowToProfilingTableNode(row)
    ptn match {
      case b: ProfilingTableBranchNode if !b.isExpanded =>
        b.expand()
        updateData()
      case _ =>
    }
  }

  def expandAllUnder(row: Int): Unit ={
    val ptn = rowToProfilingTableNode(row)
    ptn match {
      case b: ProfilingTableBranchNode =>
        b.expandAllUnder()
        updateData()
      case _ =>
    }
  }

  def expandAll(): Unit =
    profilingTableNodes.foreach {
      case tbn: ProfilingTableBranchNode => tbn.expand()
      case _ =>
    }

  private def collapse(ptn: ProfilingTableBranchNode): Unit ={
    ptn.children.foreach({
      case b: ProfilingTableBranchNode if b.isExpanded => collapse(b)
      case _ =>
    })
    ptn.collapse()
  }

  def collapse(row: Int): Unit ={
    val ptn = rowToProfilingTableNode(row)
    ptn match{
      case b: ProfilingTableBranchNode if b.isExpanded =>
        collapse(b)
        updateData()
      case _ =>
    }
  }

  def updateData(): Unit ={
    rowToProfilingTableNode = profilingTableNodes.filter(_.isDisplayed)
    for(_ <- 0 until getRowCount)this.removeRow(0)
    this.setColumnIdentifiers(Array[AnyRef]())
    this.setHeaders()
    for(r <- rowToProfilingTableNode)
      addRow(r.data().asInstanceOf[Array[Object]])
    fireTableDataChanged()
    require(rowToProfilingTableNode.length == getRowCount,
      s"Displayed ProfilingTableNodes ${rowToProfilingTableNode.length} vs nb row $getRowCount")
  }
}