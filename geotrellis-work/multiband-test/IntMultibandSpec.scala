package geotrellis.multiband

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import geotrellis._
import geotrellis.multiband._
import geotrellis.data._

@RunWith(classOf[JUnitRunner])
class IntMultibandSpec extends FunSuite {
  
  trait TestBand {
    
    val path = "core/src/main/resources/"
    val red: Raster =  GeoTiff.readRaster(path + "BAND3_RED_TOA_REF.TIF")
    val nearInfra: Raster = GeoTiff.readRaster(path + "BAND4_NIR_TOA_REF.TIF")
    val ndvi: Raster = GeoTiff.readRaster(path + "NDVI.TIF")
  }
  
  test("NDVI calculation"){
    new TestBand {
      val ob = new IntMultiBandRaster(List(red,nearInfra), red.cols, red.rows)
      assert(ob.ndvi === ndvi)
    }
  }
  

}