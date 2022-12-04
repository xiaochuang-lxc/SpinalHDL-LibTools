package libext.bus.dataBus

import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.collection.mutable.{ArrayBuffer, Queue}
import scala.util.Random.nextInt

case class DataBusShiftSimEnv(config: DataBusConfig) extends DataBusShift(config) {

  def init() = {
    io.port_in.valid #= false
    io.port_in.data #= 0
    io.port_in.empty #= 0
    io.port_in.eop #= false
    if (config.sopUsed) io.port_in.sop #= false
    io.port_out.ready #= true
    io.shift_num #= 0
  }

  def start() = {
    recvMon()
    driveShfitNum()
  }

  val refDataQueue = Queue[Array[Byte]]()

  def ByteArray2BigInt(data: Array[Byte], endianness: Endianness = LITTLE): BigInt = {
    val buffer = if (endianness == LITTLE) data.reverse.toBuffer else data.toBuffer
    buffer.prepend(0.toByte)
    BigInt(buffer.toArray)
  }

  def BigInt2ByteArray(data: BigInt, len: Int, endianness: Endianness = LITTLE): Array[Byte] = {
    val dataArray = if (endianness == LITTLE) data.toByteArray.reverse else data.toByteArray
    if (len <= dataArray.length)
      return dataArray.take(len)
    else
      return dataArray ++ Array.fill[Byte](len - dataArray.length)(0.toByte)
  }

  def sendPkg(length: Int) = {
    val data = Array.fill(length)(nextInt(128).toByte)
    refDataQueue.enqueue(data)
    val cycleNeeded = (length + config.bytesPerCycle - 1) / config.bytesPerCycle
    val unAlignedBytesNum = length & (config.bytesPerCycle - 1)
    val lastCycleEmpty = if (unAlignedBytesNum == 0) 0 else config.bytesPerCycle - unAlignedBytesNum
    val dataTmp = data ++ Array.fill(lastCycleEmpty)(0.toByte)
    println(s"[${simTime()}]:Drive $length byte to port_in need $cycleNeeded cycle,last cycle empty is ${lastCycleEmpty}")
    io.port_in.valid #= true
    for (index <- 0 until cycleNeeded) {
      io.port_in.data #= ByteArray2BigInt(data.slice(index * config.bytesPerCycle, (index + 1) * config.bytesPerCycle))
      if (config.sopUsed) io.port_in.sop #= index == 0
      io.port_in.eop #= index == (cycleNeeded - 1)
      if (index == (cycleNeeded - 1))
        io.port_in.empty #= lastCycleEmpty
      else
        io.port_in.empty #= 0
      clockDomain.waitSamplingWhere(io.port_in.ready.toBoolean & io.port_in.valid.toBoolean)
    }
    io.port_in.valid #= false
  }

  var fixShiftNum: Boolean = false
  var shiftNum: Int = config.bytesPerCycle

  def setShifNumProerty(shiftNum: Int, fixShiftNum: Boolean) = {
    this.fixShiftNum = fixShiftNum
    this.shiftNum = shiftNum
  }

  def driveShfitNum() = {
    fork {
      while (true) {
        if (fixShiftNum) io.shift_num #= shiftNum else io.shift_num #= nextInt(shiftNum + 1)
        clockDomain.waitSampling()
      }
    }
  }

  def recvMon() = {
    val dataBuffer = ArrayBuffer[Byte]()
    var pkgIndex: Int = 0
    fork {
      while (true) {
        if (io.port_out.valid.toBoolean) {
          val availDataNum = config.bytesPerCycle - io.port_out.empty.toInt
          val consumedNum = io.shift_num.toInt
          val recvNum = if (availDataNum > consumedNum) consumedNum else availDataNum
          val recvData = BigInt2ByteArray(io.port_out.data.toBigInt, config.bytesPerCycle).take(recvNum)
          println(s"DebugInfo[${simTime()}]:pkgIndex:${pkgIndex},recvLength:${recvNum},availDataNum$availDataNum,consumedNum:$consumedNum")
          dataBuffer ++= recvData
          if (io.port_out.eop.toBoolean & (consumedNum >= availDataNum)) {
            println(s"[${simTime()}]:pkgIndex:${pkgIndex} recv a pkg ,length:${dataBuffer.length}")
            val refData = refDataQueue.dequeue()
            assert(refData.length == dataBuffer.length, s"Send a ${refData.length} Byte Pkg ,recv a ${dataBuffer.length} Byte Pkg")
            for (index <- 0 until refData.length) {
              assert(refData(index) == dataBuffer(index), s"index ${index} mismatch refData=${refData(index)},recvData=${dataBuffer(index)}")
            }
            pkgIndex = pkgIndex + 1
            dataBuffer.clear()
          }
        }
        clockDomain.waitSampling()
      }
    }
  }


}
