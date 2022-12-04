package libext.bus.dataBus

import spinal.core._
import spinal.lib._

/**
 * 功能描述
 * port_in为输入端口，port_out为输出端口，shift_num为port_out每拍消耗的数据(以字节为单位)
 * 当前拍port_out消耗shift_num数据，则下一拍所有的数据移位相应字节数，出了最后一拍，需保证port_out每拍所有字节均有效
 * 使用场景:sop信号不使用,port_out empty信号无使用意义
 * shift_num最大只能为bytesPerCycle
 *
 * @param dataBusConfig
 */
class DataBusShift(dataBusConfig: DataBusConfig) extends Component {
  val io = new Bundle {
    val port_in = slave(DataBus(dataBusConfig))
    val port_out = master(DataBus(dataBusConfig.copy(sopUsed = false)))
    val shift_num = in UInt (log2Up(dataBusConfig.bytesPerCycle) + 1 bits) //指定当前cycle port_out能够消耗的字节数，该值不能大于bytesPerCycle
  }
  noIoPrefix()
  val byte_consumed = Reg(UInt(io.shift_num.getBitsWidth bits)) init (dataBusConfig.bytesPerCycle) //当前消耗字节数，当该值与shift_num累加超过dataBusConfig.bytesPerCycle时表明port_in消耗一拍数据
  val byte_consumed_tmp = byte_consumed(byte_consumed.getBitsWidth - 2 downto 0)
  val data_pending = RegNextWhen(io.port_in.data, io.port_in.fire)
  val byte_consumed_next = byte_consumed.expand + io.shift_num
  val consumed_one_beat = byte_consumed_next > dataBusConfig.bytesPerCycle //表明port_in此时可消耗一拍数据
  val avail_byte_num = dataBusConfig.bytesPerCycle * 2 - io.port_in.empty - byte_consumed
  val data_consumed_done = avail_byte_num <= io.shift_num
  when(io.port_out.fire) {
    when(io.port_out.eop) {
      byte_consumed := dataBusConfig.bytesPerCycle
    } otherwise {
      byte_consumed := Mux(
        sel = consumed_one_beat & (!io.port_in.last),
        //若消耗一拍数据且此时port_in还不是最后一拍数据，那么消耗一拍数据需减去bytesPerCycle以调整偏差
        whenTrue = byte_consumed_next.msb ? U(dataBusConfig.bytesPerCycle, byte_consumed.getBitsWidth bits) | byte_consumed_next(byte_consumed.getBitsWidth - 2 downto 0).resized, //byte_consumed_next-dataBusConfig.bytesPerCycle
        //若port_in为最后一拍，说明下一拍port_in无法进新的数据（若此时消耗一拍数据会走上面的if语句），此时截取对应位
        whenFalse = byte_consumed_next.resized
      )
    }
  }
  //last 信号处理
  io.port_out.eop := io.port_in.eop & data_consumed_done
  //ready 信号处理
  io.port_in.ready := Mux(sel = io.port_in.last, whenTrue = io.port_out.ready & data_consumed_done, whenFalse = io.port_out.ready & consumed_one_beat)
  //valid信号处理
  io.port_out.valid := io.port_in.valid
  //empty 信号处理
  io.port_out.empty := Mux(
    sel = avail_byte_num(avail_byte_num.getBitsWidth - 2, 2 bits).orR, //avail_byte_num>dataBusConfig.bytesPerCycle,
    whenTrue = U(0, io.port_in.empty.getBitsWidth bits),
    whenFalse = (dataBusConfig.bytesPerCycle - avail_byte_num).resized
  )
  //data 信号处理
  io.port_out.data:= ((io.port_in.data##data_pending)>>byte_consumed@@U(0,3 bits)).resized
  /*val data_tmp = Mux(
    sel = byte_consumed.msb & io.port_in.last,
    whenTrue = data_pending ## io.port_in.data, //移位采用byte_consumed_tmp，在port_in尾拍时若出现byte_consumed大于bytesPerCycle，则需要调转拼接排序
    whenFalse = io.port_in.data ## data_pending //其他情况下不会出现byte_consumed>bytesPerCycle的情况
  )
  switch(byte_consumed_tmp) {
    for (index <- 1 until dataBusConfig.bytesPerCycle) {
      is(index) {
        io.port_out.data := data_tmp(index * 8, dataBusConfig.dataWidth bits)
      }
    }
    default(io.port_out.data := io.port_in.data) //byte_consumed为bytesPerCycle场景
  }*/
}

object DataBusShiftApp extends App {

  SpinalSystemVerilog(new DataBusShift(DataBusConfig(512, true)))
}