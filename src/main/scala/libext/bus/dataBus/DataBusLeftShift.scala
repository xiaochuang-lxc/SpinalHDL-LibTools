package libext.bus.dataBus

import spinal.core._
import spinal.lib._

/**
 * port_in.user 低logUp2(dataBusConfig.bytesPerCycle)比特存放其首拍应移动字符数，在整包数据中该信息需保持不变
 * port_out 首拍空出指定字节数后送至port_out
 *
 * @param dataBusConfig
 */
case class DataBusLeftShift(dataBusConfig: DataBusConfig) extends Component {
  val io = new Bundle {
    val port_in = slave(DataBus(dataBusConfig))
    val port_out = master(DataBus(dataBusConfig.copy(sopUsed = false)))
  }
  noIoPrefix()
  assert(dataBusConfig.userWidth >= log2Up(dataBusConfig.bytesPerCycle), s"the userWidth is not enough")
  val data_tmp = RegNextWhen(io.port_in.data, io.port_out.fire)
  val offset = io.port_in.user(0, log2Up(dataBusConfig.bytesPerCycle) bits).asUInt //首拍空字节数
  val shift_num = U(dataBusConfig.bytesPerCycle, log2Up(dataBusConfig.bytesPerCycle) + 1 bits) - offset //移位字节数
  val last_cycle_need_delay = offset > io.port_in.empty //判定port_in最后一拍数据需要一拍还是两拍来处理
  val last_cycle_cnt = Reg(Bool()) init (False)
  val data_shift_value = (io.port_in.data ## data_tmp) |>> (shift_num @@ U(0, 3 bits))
  io.port_out.valid := io.port_in.valid
  io.port_out.data := data_shift_value.resized
  io.port_out.empty := io.port_out.eop ? (io.port_in.empty - offset) | U(0, io.port_out.empty.getBitsWidth bits)
  io.port_out.user := io.port_in.user
  io.port_out.eop := io.port_in.eop & (last_cycle_cnt === last_cycle_need_delay)
  when(io.port_in.last & io.port_out.fire) {
    last_cycle_cnt := !io.port_out.eop
  }
  io.port_in.ready := Mux(io.port_in.eop, io.port_out.last & io.port_out.ready, io.port_out.ready)
}

object DataBusLeftShiftApp extends App {
  SpinalSystemVerilog(new DataBusLeftShift(DataBusConfig(512, true, 6)))
}
