package oscar.cbls.lib.search.multiObjective

import oscar.visual._
import javax.swing.SwingUtilities
import oscar.visual.plot.PlotScatter

/**
 * @author Pierre Schaus
 */
class PlotPareto(
                  title: String = "Pareto",
                  xlab: String = "Obj1",
                  ylab: String = "Obj2",
                ) extends  PlotScatter(title, xlab, ylab, 2){ //extends VisualFrame(title,1,1){


//  this.add(s)
//  this.pack()

  def reDrawPareto(paretoPoints:Iterable[(Long,Long)], oldParetoPoints:Option[Iterable[(Long,Long)]] = None): Unit ={

    SwingUtilities.invokeLater(new Runnable() {
      def run() {

        removeAllPoints(1)
        oldParetoPoints match{
          case None => ;
          case Some(old) =>
            for (p <- old) {
              addPoint(p._1, p._2, 1)
            }
        }

        removeAllPoints(0)
        for (p <- paretoPoints) {
          addPoint(p._1, p._2, 0)
        }
      }
    })
  }
}
