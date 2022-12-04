package libext.bus.dataBus

import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._

class DataBusShiftTest extends AnyFunSuite {
  val dutCompiled = SimConfig.withFstWave.compile(DataBusShiftSimEnv(DataBusConfig(512, true)))
  test("包长递增，每拍移出固定字节长度") {
    dutCompiled.doSim { dut =>
      dut.init()
      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitSampling(10)
      dut.start()
      var pkgIndex = 0
      for (length <- 1 to 256) {
        for (shiftNum <- 1 to 64) {
          println(s"[${simTime()}]:pkgIndex:${pkgIndex},test $length byte pkg ,with fixShiftNum:${shiftNum}")
          dut.setShifNumProerty(shiftNum, true)
          dut.sendPkg(length)
          pkgIndex = pkgIndex + 1
        }
      }
      dut.clockDomain.waitSampling(10)
      assert(dut.refDataQueue.isEmpty)
    }
  }

  test("包长递增，每拍移出字节长度不固定") {
    dutCompiled.doSim { dut =>
      dut.init()
      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitSampling(10)
      dut.start()
      var pkgIndex = 0
      for (length <- 1 to 2048 by 16) {
        for (shiftNum <- 1 to 64) {
          println(s"[${simTime()}]:pkgIndex:${pkgIndex},test $length byte pkg ,with fixShiftNum:${shiftNum}")
          dut.setShifNumProerty(shiftNum, false)
          dut.sendPkg(length)
          pkgIndex = pkgIndex + 1
        }
      }
      dut.clockDomain.waitSampling(10)
      assert(dut.refDataQueue.isEmpty)
    }
  }
}
