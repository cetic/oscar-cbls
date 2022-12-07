package oscar.cbls.visual.profiling

import oscar.cbls.core.search.{Neighborhood, NeighborhoodProfiler}
import oscar.cbls.visual.SingleFrameWindow

import java.awt.{BorderLayout, Dimension, GridBagLayout}
import javax.swing.{JFrame, JPanel, JScrollPane}

object VisualProfiler{
  def showProfile(search: Neighborhood, title: String = "Visual Profiler"): Unit ={
    new VisualProfiler(search,title)
    //SingleFrameWindow.show(visualProfiler,title)
  }
}

class VisualProfiler(search: Neighborhood, title: String) extends JFrame{
  this.setTitle(title)
  this.setLayout(new GridBagLayout())
  this.setPreferredSize(new java.awt.Dimension(1600,400))

  val profilingTree = new ProfilingTree(search)
  //profilingTree.setPreferredSize(new Dimension(200,400))
  profilingTree.draw()
  val profilingTreeScroll = new JScrollPane(profilingTree,javax.swing.ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,javax.swing.ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED)

  val allSimpleNeighborhoodProfiler: List[NeighborhoodProfiler] =
    profilingTree.allProfilingNodes.filter(_.hasChildren == false).map(_.profiler.asInstanceOf[NeighborhoodProfiler])
  val profilingTable: ProfilingTable = ProfilingTable(allSimpleNeighborhoodProfiler)
  //profilingTable.setPreferredSize(new Dimension(400,400))

  import java.awt.GridBagConstraints

  val gbc: GridBagConstraints = new GridBagConstraints
  gbc.anchor = GridBagConstraints.FIRST_LINE_START
  gbc.fill = GridBagConstraints.BOTH
  gbc.weightx = 0.4
  gbc.weighty = 1.0
  gbc.gridx = 0
  gbc.gridy = 0
  this.add(profilingTreeScroll, gbc)

  gbc.weightx = 0.6
  gbc.gridx = 1
  this.add(profilingTable, gbc)



  profilingTree.getSize()
  profilingTable.getSize()

  this.setResizable(true)
  this.pack()
  this.revalidate()
  this.setVisible(true)
  this.setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE)
}
