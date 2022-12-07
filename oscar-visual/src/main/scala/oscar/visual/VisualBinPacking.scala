package oscar.visual

import oscar.visual.shapes.VisualRectangle
import java.awt.geom.Rectangle2D
import java.awt.Color

class VisualBinPacking(binWidth: Int, autoRepaint: Boolean) extends VisualDrawing(true, false) {
  
  private var items: List[VisualBinItem] = List()
  
  def addItem(bin: Int, height: Int, color: Color = VisualUtil.getRandomColor(true)): VisualBinItem = {
    val item = new VisualBinItem(bin, height)
    item.autoRepaint = autoRepaint
    item.innerCol = color
    redraw(bin)
    items = item :: items
    item
  }
  
  private def redraw(bin: Int): Unit = {
    var y: Double = 0
    for (item <- items) {
      if (item.bin == bin) {
        item.translate(bin*binWidth, y)
        y += item.height
      }      
    }
  }
  
  class VisualBinItem(b: Int, val h: Int) extends VisualRectangle(this, new Rectangle2D.Double(b*binWidth, 0, binWidth, h)) {   
    private var _bin = b
    def bin: Int = _bin
    def bin_=(bin: Int): Unit = {
      val oldBin = _bin
      _bin = bin
      translate(binWidth*bin, y)
      redraw(oldBin)
      redraw(bin)
    }
  }
}

object VisualBinPacking {
  
  def apply(binWidth: Int = 50, autoRepaint: Boolean = true): VisualBinPacking = {
    new VisualBinPacking(binWidth, autoRepaint)
  }
}

object VisualBinPackingExample extends App {
  
  val frame = VisualFrame("Example")
  val inFrame = frame.createFrame("Bin Packing")
  val binPacking = VisualBinPacking()  
  inFrame.add(binPacking)
  inFrame.pack()
   
  val rand = new scala.util.Random
  val items = Array.tabulate(20)(i => binPacking.addItem(i, rand.nextInt(91) + 10))
  
  for (_ <- 0 until 100) {
    val item = rand.nextInt(items.size)
    val bin = rand.nextInt(10)
    items(item).bin = bin
    Thread.sleep(500)
  }
}
