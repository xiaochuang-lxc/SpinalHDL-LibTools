package libext.bus.dataBus

import spinal.core._
import spinal.lib._

/**
 * 功能描述：
 * port_in.user 低logUp2(dataBusConfig.bytesPerCycle)比特存放其首拍空字节数(LSB)，在整包数据中该信息需保持不变
 * 将port_in首拍空字节数排除掉后送至port_out,port_out仅最后一排empry允许非0值
 *
 * @param dataBusConfig
 */
case class DataBusRightShift(dataBusConfig: DataBusConfig) extends Component {
  val io = new Bundle {
    val port_in = slave(DataBus(dataBusConfig))
    val port_out = master(DataBus(dataBusConfig.copy(sopUsed = false)))
  }
  noIoPrefix()
  assert(dataBusConfig.userWidth >= log2Up(dataBusConfig.bytesPerCycle), s"the userWidth is not enough")
  val offset = io.port_in.user(0, log2Up(dataBusConfig.bytesPerCycle) bits).asUInt //首拍空字节数
  val data_dly = RegNextWhen(io.port_in.data, io.port_in.fire)
  val user_dly = RegNextWhen(io.port_in.user, io.port_in.fire)
  val empty_dly = RegNextWhen(io.port_in.empty, io.port_in.fire)
  val eop_dly = RegNextWhen(io.port_in.eop, io.port_in.fire)
  val pending_flag = RegInit(False)
  val offset_dly = user_dly(0, log2Up(dataBusConfig.bytesPerCycle) bits).asUInt
  val shift_num = UInt(log2Up(dataBusConfig.bytesPerCycle) + 1 + 3 bits)
  val last_cycle_empty = offset.expand + io.port_in.empty
  //pending flag process
  when(io.port_in.firstFire) {
    pending_flag := pending_flag
  } elsewhen (io.port_in.lastFire & (!io.port_in.first) & (offset =/= 0) & (!last_cycle_empty.msb)) {
    pending_flag.set()
  } elsewhen (io.port_out.lastFire) {
    pending_flag.clear()
  }
  //port_in ready处理
  io.port_in.ready := Mux(
    sel = (offset =/= 0) & io.port_in.first & (!io.port_in.eop) & (!pending_flag), //多拍报文首拍，且offset不为0
    whenTrue = True,
    whenFalse = io.port_out.ready
  )
  //port_out valid处理
  io.port_out.valid := (pending_flag & eop_dly) || (Mux(
    sel = io.port_in.first,
    whenTrue = (io.port_in.eop | (offset === 0)) & io.port_in.valid,
    whenFalse = io.port_in.valid
  ))
  //shift num计算
  when(pending_flag) {
    shift_num := offset_dly.expand @@ U(0, 3 bits)
  } otherwise {
    shift_num := ((io.port_in.first & io.port_in.eop) | (offset === 0)).asUInt @@ offset @@ U(0, 3 bits)
  }
  //数据移位
  val data_shift_value = (io.port_in.data ## data_dly) |>> shift_num
  io.port_out.data := data_shift_value.resized
  //eop
  io.port_out.eop := (pending_flag & eop_dly) | Mux(
    sel = (offset =/= 0) & (!io.port_in.first),
    whenTrue = io.port_in.eop & last_cycle_empty.msb,
    whenFalse = io.port_in.eop & (!pending_flag))
  //user
  io.port_out.user := Mux(sel = pending_flag, whenTrue = user_dly, whenFalse = io.port_in.user)
  //empty
  io.port_out.empty := Mux(
    sel = io.port_out.eop,
    whenTrue = Mux(
      sel = pending_flag & eop_dly,
      whenTrue = offset_dly + empty_dly,
      whenFalse = last_cycle_empty.resize(io.port_out.empty.getBitsWidth bits)
    ),
    whenFalse = U(0, log2Up(dataBusConfig.dataWidth / 8) bits)
  )
}

object DataBusRightShiftApp extends App {
  SpinalSystemVerilog(new DataBusRightShift(DataBusConfig(512, true, 6)))
}