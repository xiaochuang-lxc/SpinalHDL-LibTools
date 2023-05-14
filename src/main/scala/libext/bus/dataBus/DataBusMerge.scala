package libext.bus.dataBus

import spinal.core._
import spinal.lib._

/**
 * 针对port_in存在非尾拍时empty不为0场景排出气泡合并输出
 * 要求：port_in中对于empty不为0的cycle，要求对应的empty字节需为0
 * port_out:仅尾拍允许empty不为0
 *
 * @param dataBusConfig
 */
class DataBusMerge(dataBusConfig: DataBusConfig) extends Component {
  val io = new Bundle {
    val port_in = slave(DataBus(dataBusConfig))
    val port_out = master(DataBus(dataBusConfig.copy(sopUsed = false)))
  }
  noIoPrefix()
  val avail_num = Reg(UInt(log2Up(dataBusConfig.bytesPerCycle) + 1 bits)) init (0) //当前待发射有效字节数
  val data_tmp = Reg(Bits(dataBusConfig.dataWidth bits)) init (0)
  val pending_flag = RegInit(False)
  val seat_enough = io.port_in.empty > avail_num //port_in 输入字节数小于可容纳字节数
  val seat_equal = io.port_in.empty === avail_num //port_in 输入字节数等于可容纳字节数
  val seat_less = io.port_in.empty < avail_num //port_in 输入字节数大于可容纳字节数
  val fire_need = (seat_less | seat_equal) || io.port_in.eop //当输入字节数大于等于可容纳字节数时需port_out发送数据
  val data_shifted = (B(0, dataBusConfig.dataWidth bits) ## io.port_in.data) |<< (avail_num @@ U(0, 3 bits))
  //avali_num设置
  when(io.port_in.eop) {
    when(io.port_out.fire) {
      when(seat_less & (~pending_flag)) {
        data_tmp := data_shifted(dataBusConfig.dataWidth, dataBusConfig.dataWidth bits)
        avail_num := avail_num - io.port_in.empty
      } otherwise {
        data_tmp.clearAll()
        avail_num.clearAll()
      }
    }
  } otherwise { //非port_in尾拍情况
    when(io.port_in.fire) {
      when(seat_less | seat_equal) {
        data_tmp := data_shifted(dataBusConfig.dataWidth, dataBusConfig.dataWidth bits)
        avail_num := avail_num - io.port_in.empty
      } otherwise {
        data_tmp := data_tmp | data_shifted(0, dataBusConfig.dataWidth bits)
        avail_num := (U(1, 1 bits) @@ avail_num(0, log2Up(dataBusConfig.bytesPerCycle) bits)) - io.port_in.empty
      }
    }
  }
  //pending_flag设置
  when(io.port_out.fire & io.port_in.eop) {
    pending_flag := seat_less & (~pending_flag)
  }

  io.port_out.valid := fire_need & io.port_in.valid
  io.port_in.ready := Mux(
    sel = io.port_in.eop,
    whenTrue = Mux(pending_flag, True, (seat_equal | seat_enough)) & io.port_out.ready,
    whenFalse = Mux(seat_less | seat_equal, io.port_out.ready, True)
  )
  io.port_out.empty := Mux(
    sel = io.port_out.eop,
    whenTrue = (pending_flag ? U(0, log2Up(dataBusConfig.bytesPerCycle) bits) | io.port_in.empty) - avail_num(0, log2Up(dataBusConfig.bytesPerCycle) bits),
    whenFalse = U(0, log2Up(dataBusConfig.bytesPerCycle) bits))
  io.port_out.user := io.port_in.user
  io.port_out.eop := io.port_in.eop & (pending_flag | (~seat_less))
  io.port_out.data := Mux(sel = pending_flag, whenTrue = data_tmp, whenFalse = data_tmp | data_shifted(0, dataBusConfig.dataWidth bits))
}

object DataBusMergeApp extends App {
  SpinalConfig(nameWhenByFile = false).generateSystemVerilog(new DataBusMerge(DataBusConfig(512, false, 16)))
}