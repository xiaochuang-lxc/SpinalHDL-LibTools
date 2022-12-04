package libext.bus.dataBus

import spinal.core._
import spinal.lib._

case class DataBusConfig(dataWidth: Int, sopUsed: Boolean = true) {
  val bytesPerCycle: Int = dataWidth / 8
}

case class DataBusPayload(dataWidth: Int, sopUsed: Boolean = true) extends Bundle {
  val eop: Bool = Bool()
  val sop: Bool = sopUsed generate Bool()
  val data: Bits = Bits(dataWidth bits)
  val empty: UInt = UInt(log2Up(dataWidth / 8) bits) //仅eop时允许非0，表示最后一拍有多少无效数据
}

class DataBus(config: DataBusConfig) extends Stream(DataBusPayload(config.dataWidth, config.sopUsed)) {

  def first: Bool = signalCache(this, "first")(if (config.sopUsed) payload.sop else RegNextWhen(payload.eop, fire, True).setCompositeName(this, "first", true))

  def last: Bool = payload.eop

  def lastFire = last & fire

  def firstFire = first & fire
}

object DataBus {

  def rename(bus: DataBus): Unit = {
    bus.flatten.foreach((bt) => {
      bt.setName(bt.getName().replace("payload_", ""))
      if (bt.getName().startsWith("io_")) bt.setName(bt.getName().replaceFirst("io_", ""))
    })
  }

  def apply(config: DataBusConfig): DataBus = {
    val bus = new DataBus(config)
    if (Component.current == bus.component)
      bus.component.addPrePopTask(() => {
        rename(bus)
      })
    else
      rename(bus)
    bus
  }

  def apply(dataWidth: Int, sopUsed: Boolean = true): DataBus = {
    apply(DataBusConfig(dataWidth, sopUsed))
  }
}
