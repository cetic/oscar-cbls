package oscar.cbls.visual

import java.awt.image.BufferedImage
import java.awt.{BorderLayout, Dimension}
import java.io.File
import javax.imageio.ImageIO
import javax.swing.{JFrame, JPanel, JScrollPane}

object SingleFrameWindow{
  def show(panel:JPanel, title:String, width:Int = 960, height:Int = 960, backgroundPanel: Option[JPanel] = None):SingleFrameWindow = {
    new SingleFrameWindow(panel:JPanel,title:String,width,height,backgroundPanel)
  }

  def showFrame(frame:JFrame,title:String,width:Int = 960,height:Int = 960): Unit = {
    frame.setTitle(title)
    frame.setLayout(new BorderLayout())
    frame.setPreferredSize(new java.awt.Dimension(width,height))
    frame.pack()
    frame.revalidate()
    frame.setVisible(true)
  }
}

// Display a custom panel in a frame window
class SingleFrameWindow(val panel:JPanel, title:String, width:Int, height:Int, backgroundPanel: Option[JPanel] = None){

  panel.setPreferredSize(new Dimension(width,height))
  val frame = new JFrame()
  frame.setTitle(title)
  frame.setPreferredSize(new java.awt.Dimension(width,height))
  val scrollablePanel = new JScrollPane(panel,javax.swing.ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,javax.swing.ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED)

  if(backgroundPanel.isDefined) {
    frame.setContentPane(backgroundPanel.get)
    frame.setGlassPane(scrollablePanel)
    scrollablePanel.setOpaque(false)
    scrollablePanel.setVisible(true)
  } else {
    frame.add(scrollablePanel, BorderLayout.CENTER)
  }

  frame.setResizable(true)
  frame.pack()
  frame.revalidate()
  frame.setVisible(true)
  frame.setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE)

  def saveWindowAsPng(savingFile: File): Unit ={
    val bi = new BufferedImage(panel.getWidth, panel.getHeight, BufferedImage.TYPE_INT_ARGB)
    val g = bi.createGraphics
    panel.paintAll(g) //this == JComponent

    try
      ImageIO.write(bi,"png", savingFile)
    catch {
      case e: Exception =>
        println("WARNING : Error while saving drawing into png file : " + e.getMessage)
    }
  }

  def close(): Unit ={
    frame.dispose()
  }
}
