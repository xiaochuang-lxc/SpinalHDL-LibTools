package libext.bitmap

import org.scalatest.FunSuite
import spinal.core.sim._
import scala.collection.mutable.ArrayBuffer
case class BitMaoSimEnv(bitmapNum:Int,ramDataWidth:Int) extends BitMap(bitmapNum = bitmapNum,ramDataWidth=ramDataWidth){
  bitmapMem.mem.simPublic()
  val sendBitMap=ArrayBuffer[Int]()
  def sendTag(id:Int)={
    io.cmpl.valid#=true
    io.cmpl.payload#=id
    clockDomain.waitSampling()
    io.cmpl.valid#=false
    sendBitMap.append(id)
  }

  def init()={

    io.cmpl.valid#=false
    io.avail.ready#=true
  }

  def mon()={
    var targetId=0
    while(true){
      if(io.avail.valid.toBoolean && io.avail.ready.toBoolean){
        assert(targetId==io.avail.payload.toInt)
        assert(sendBitMap.contains(io.avail.payload.toInt))
        sendBitMap.remove(sendBitMap.indexWhere(_==targetId))
        targetId=targetId+1
        if(targetId==bitmapNum) {
          targetId = 0
        }
      }
      clockDomain.waitSampling()
    }
  }
}
class BitMapTest extends FunSuite {
  test("normal test"){
    SimConfig.withFstWave.compile(BitMaoSimEnv(1024,16)).doSim{dut=>
      dut.init()
      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitSampling(10)
      fork(dut.mon())
      for(index<- 0 until 1024){
        dut.sendTag(index)
      }
      dut.clockDomain.waitSampling(100)
    }
  }
  test("reverse test"){
    SimConfig.withFstWave.compile(BitMaoSimEnv(1024,16)).doSim{dut=>
      dut.init()
      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitSampling(10)
      fork(dut.mon())
      for(index<- 1023 to 0 by -1){
        dut.sendTag(index)


      }
      dut.clockDomain.waitSampling(2048)
    }
  }
  test("halt test"){
    SimConfig.withFstWave.compile(BitMaoSimEnv(1024,16)).doSim{dut=>
      dut.init()
      for(index<-0 until dut.bitmapNum/dut.ramDataWidth){
        setBigInt(dut.bitmapMem.mem,index,0)
      }
      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitSampling(10)
      dut.io.avail.ready#=false
      fork(dut.mon())
      for(index<- 0 to 1023){
        dut.sendTag(index)
        if(index<32){
          dut.io.avail.ready#=false
        }else{
          dut.io.avail.ready#=true
        }
        dut.clockDomain.waitSampling()
      }
      dut.clockDomain.waitSampling(2048)
    }
  }
}
