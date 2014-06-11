package geotrellis.multiband

import geotrellis._

class IntMultiBandRaster(val rBand: List[Raster], val cols: Int, val rows: Int) {

  def ndvi(): Raster = {

    if (rBand.length < 2)
      throw new Error("rBand.OutOfElements")
    else if (!checkDimensions(rBand))
      throw new Error("rBand. diamensions are not equal")
    else {
      val rRed = getElement(rBand, 0)
      val rNearInfra = getElement(rBand, 1)

      (rRed - rNearInfra) / (rRed + rNearInfra)
    }
  }

  def checkDimensions(ras: List[Raster]): Boolean = {

    if (ras.isEmpty)
      true
    else
      (ras.head.cols == cols && ras.head.rows == rows) && (checkDimensions(ras.tail))
  }

  def getElement(list: List[Raster], index: Int): Raster = {

    if (list.isEmpty && index > 0)
      throw new Error("List.IndexOutOfBound")
    else if (list.isEmpty)
      throw new Error("List.Empty")
    else if (index == 0)
      list.head
    else
      getElement(list.tail, index - 1)

  }
}