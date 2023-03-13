package oscar.cbls.visual.profiling

import oscar.cbls.core.search.Neighborhood
import oscar.cbls.visual.SingleFrameWindow

import java.awt.{BorderLayout, Dimension, GraphicsDevice, GraphicsEnvironment, GridBagLayout, Toolkit}
import javax.swing.{JFrame, JPanel, JScrollPane}

object VisualProfiler{
  def showProfile(search: Neighborhood, onConsole: Boolean = false, title: String = "Visual Profiler"): Unit ={
    if(onConsole)
      ProfilingConsole(search.profiler,search.profiler.collectThisProfileHeader)
    else
      new VisualProfiler(search,title)
  }
}

class VisualProfiler(search: Neighborhood, title: String) extends JFrame{
  this.setTitle(title)
  private val screenSize = Toolkit.getDefaultToolkit.getScreenSize
  this.setPreferredSize(new java.awt.Dimension(screenSize.width,screenSize.height))

  val profilingTable: ProfilingTable = ProfilingTable(search.profiler)

  this.add(profilingTable)

  profilingTable.getSize()

  this.setResizable(true)
  this.pack()
  this.revalidate()
  this.setVisible(true)
  this.setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE)
}
