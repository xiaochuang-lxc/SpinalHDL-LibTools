package libext.cache.interface

import spinal.core._
import spinal.lib._

case class MemPortConfig(addrWidht: Int, dataWidth: Int, useStrb: Boolean = true) {
}

case class MemCmdPayload(config: MemPortConfig) extends Bundle {
  val addr = UInt(config.addrWidht bits)
  val data = Bits(config.dataWidth bits)
  val wr = Bool() //True:Write,False:Read
  val strb = if (config.useStrb) Bits(config.dataWidth / 8 bits) else null
}

class MemCmd(config: MemPortConfig) extends Stream(MemCmdPayload(config)) {

}

object MemCmd {
  def rename(bus: MemCmd) = {
    bus.flatten.foreach((bt) => {
      bt.setName(bt.getName().replace("payload_", ""))
      if (bt.getName().startsWith("io_")) bt.setName(bt.getName().replaceFirst("io_", ""))
    })
  }

  def apply(config: MemPortConfig): MemCmd = {
    val bus = new MemCmd(config)
    if (Component.current == bus.component)
      bus.component.addPrePopTask(() => {
        rename(bus)
      })
    else
      rename(bus)
    bus

  }
}

case class MemRspPayload(config: MemPortConfig) extends Bundle {
  val rdata = Bits(config.dataWidth bits)
  val fault = out Bool()
}

class MemRsp(config: MemPortConfig) extends Flow(MemRspPayload(config)) {

}

object MemRsp {
  def rename(bus: MemRsp) = {
    bus.flatten.foreach((bt) => {
      bt.setName(bt.getName().replace("payload_", ""))
      if (bt.getName().startsWith("io_")) bt.setName(bt.getName().replaceFirst("io_", ""))
    })
  }

  def apply(config: MemPortConfig): MemRsp = {
    val bus = new MemRsp(config)
    if (Component.current == bus.component)
      bus.component.addPrePopTask(() => {
        rename(bus)
      })
    else
      rename(bus)
    bus

  }
}

case class MemPort(config: MemPortConfig) extends Bundle with IMasterSlave {
  val cmd = MemCmd(config)
  val rsp = MemRsp(config)

  override def asMaster(): Unit = {
    master(cmd)
    slave(rsp)
  }
}
