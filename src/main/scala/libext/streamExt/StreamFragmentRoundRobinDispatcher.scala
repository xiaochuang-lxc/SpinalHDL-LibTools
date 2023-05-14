package libext.streamExt

import spinal.core._
import spinal.lib._

case class StreamFragmentRoundRobinDispatcher[T <: Data](dataType: HardType[T], slavePortNumber: Int) extends Component {
  require(slavePortNumber >= 2, s"SlavePortNumber $slavePortNumber is less than 2")
  val io = new Bundle {
    val port_in = slave Stream (Fragment(dataType))
    val port_out = Vec(master Stream (Fragment(dataType)), slavePortNumber)
    val port_sel_oh = out Bits (slavePortNumber bits)
  }
  val mask_locked = Vec(Reg(Bool()), slavePortNumber)
  val mask_arbited = Vec(Bool(), slavePortNumber)
  val port_locked = RegInit(False)
  val mask_route = port_locked ? mask_locked | mask_arbited

  port_locked.setWhen(io.port_in.ready)
  port_locked.clearWhen(io.port_in.lastFire)
  for (index <- mask_locked.range) {
    mask_locked(index) init (index == slavePortNumber - 1)
  }

  mask_arbited := OHMasking.roundRobin(Vec(io.port_out.map(_.ready)), Vec(mask_locked.last +: mask_locked.take(slavePortNumber - 1)))
  when(io.port_in.ready) {
    mask_locked := mask_arbited
  }

  (io.port_out, mask_route).zipped.foreach((port, sel) => {
    port.valid := io.port_in.valid & sel
    port.payload := io.port_in.payload
  })
  io.port_in.ready := (io.port_out, mask_route).zipped.map(_.ready & _).reduce(_ | _)
  io.port_sel_oh := mask_route.asBits
}

object StreamFragmentRoundRobinDispatcherApp extends App {
  SpinalSystemVerilog(StreamFragmentRoundRobinDispatcher(Bits(8 bits), 2))
}