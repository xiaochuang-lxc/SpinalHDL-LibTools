package libext.cache.interface

import spinal.core._
import spinal.lib._

case class UserPortConfig(addrWidht: Int, dataWidth: Int, useStrb: Boolean = true) {
}

case class UserCmdPayload(config: UserPortConfig) extends Bundle {
  val addr = UInt(config.addrWidht bits)
  val data = Bits(config.dataWidth bits)
  val wr = Bool() //True:Write,False:Read
  val strb = if (config.useStrb) Bits(config.dataWidth / 8 bits) else null
  val fulsh=Bool()
}

class UserCmd(config: UserPortConfig) extends Stream(UserCmdPayload(config)) {

}

object UserCmd {
  def rename(bus: UserCmd) = {
    bus.flatten.foreach((bt) => {
      bt.setName(bt.getName().replace("payload_", ""))
      if (bt.getName().startsWith("io_")) bt.setName(bt.getName().replaceFirst("io_", ""))
    })
  }

  def apply(config: UserPortConfig): UserCmd = {
    val bus = new UserCmd(config)
    if (Component.current == bus.component)
      bus.component.addPrePopTask(() => {
        rename(bus)
      })
    else
      rename(bus)
    bus

  }
}

case class UserRspPayload(config: UserPortConfig) extends Bundle {
  val rdata = Bits(config.dataWidth bits)
  val fault = out Bool()
}

class UserRsp(config: UserPortConfig) extends Flow(UserRspPayload(config)) {

}

object UserRsp {
  def rename(bus: UserRsp) = {
    bus.flatten.foreach((bt) => {
      bt.setName(bt.getName().replace("payload_", ""))
      if (bt.getName().startsWith("io_")) bt.setName(bt.getName().replaceFirst("io_", ""))
    })
  }

  def apply(config: UserPortConfig): UserRsp = {
    val bus = new UserRsp(config)
    if (Component.current == bus.component)
      bus.component.addPrePopTask(() => {
        rename(bus)
      })
    else
      rename(bus)
    bus

  }
}

case class UserPort(config: UserPortConfig) extends Bundle  with IMasterSlave {
  val cmd = UserCmd(config)
  val rsp = UserRsp(config)

  override def asMaster(): Unit = {
    master(cmd)
    slave(rsp)
  }
}

