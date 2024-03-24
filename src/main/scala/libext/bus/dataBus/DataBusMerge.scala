package libext.bus.dataBus

import spinal.core._
import spinal.lib._

/**
 * 针对port_in存在非尾拍时empty不为0场景排出气泡合并输出
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
  val data_tmp = Reg(Bits(dataBusConfig.dataWidth bits))
  val package_pending = RegInit(False) //用于标志当port_in eop为1时当拍无法将所有数据从port_out输出
  val seat_equal = io.port_in.empty === avail_num //port_in 输入字节数等于可容纳字节数，可向下游发射
  val seat_less = io.port_in.empty < avail_num //port_in 输入字节数大于可容纳字节数
  val port_in_fire = io.port_in.fire
  val port_out_fire = io.port_out.fire

  //val eop_pending=RegNextWhen(io.port_in.eop,port_in_fire)
  val user_pending = (dataBusConfig.userWidth > 0) generate RegNextWhen(io.port_in.user, port_in_fire)

  //package_pending 设置
  when(port_in_fire) {
    when(io.port_in.last) {
      package_pending := seat_less || package_pending //在报文尾拍时，若出现seat_less，则表明eop会delay输出。而若package_pending已为高，说明上一个package 已出现pending，当前报文为单拍报文
    } otherwise {
      package_pending.clear()
    }
  } elsewhen (port_out_fire) { //出现port_out fire而port_in fire为0，意味这此时port_in valid为低，此时为pending报文输出
    package_pending.clear()
  }

  //avail_num设置
  val avail_num_cal0 = avail_num - io.port_in.empty //avail_num+dataBusConfig.bytesPerCycle-io.port_in.empty-dataBusConfig.bytesPerCycle
  val avail_num_cal1 = U(dataBusConfig.bytesPerCycle, avail_num.getBitsWidth bits) - io.port_in.empty
  val avail_num_cal2 = (U(1, 1 bits) @@ avail_num(0, log2Up(dataBusConfig.bytesPerCycle) bits)) - io.port_in.empty
  when(port_in_fire) {
    when(io.port_in.last) { //报文尾拍，也包含单拍报文的可能性
      avail_num := Mux(
        sel = package_pending,
        whenTrue = avail_num_cal1, //对应当前报文为单拍报文，此时上一拍报文出现package_pending,当前拍输出只能输出上一拍pending数据，此拍数据pending
        whenFalse = Mux(
          sel = seat_less,
          whenTrue = avail_num_cal0, //当前报文会出现package pending，pending的字节数为(dataBusConfig.bytesPerCycle-io.port_in.empty+avail_num-dataBusConfig.bytesPerCycle)
          whenFalse = U(0, avail_num.getBitsWidth bits) //当前报文能够在当前拍消耗完，清零
        )
      )
    } elsewhen (io.port_in.first) { //报文首拍
      avail_num := Mux(
        sel = package_pending,
        whenTrue = avail_num_cal1, //对应当前报文为单拍报文，此时上一拍报文出现package_pending,当前拍输出只能输出上一拍pending数据，此拍数据pending
        whenFalse = Mux(
          sel =  seat_equal,
          whenTrue = avail_num_cal0, //port_out消耗一拍报文，此时pending报文字节数为为(dataBusConfig.bytesPerCycle-io.port_in.empty+avail_num-dataBusConfig.bytesPerCycle)
          whenFalse = avail_num_cal1 ///报文首拍,且无法送到下游，avail_num累加
        )
      )
    } otherwise { //中间拍报文
      avail_num := Mux(
        sel = seat_equal | seat_less,
        whenTrue = avail_num_cal0, //报文消耗
        whenFalse = avail_num_cal2 //当前拍port_in消耗，port_out却不输出:avail_num+dataBusConfig.bytesPerCycle-io.port_in.empty 此时avail_num最高bit为0，可进行合并
      )
    }
  } elsewhen (port_out_fire) { //port in 无数据，port_out 输出消耗pending数据
    avail_num.clearAll()
  }

  //数据合并移位
  val data_shifted = Bits(dataBusConfig.dataWidth * 2 bits)
  switch(avail_num) { //相较于采用移位，更节省资源
    for (index <- 0 until dataBusConfig.bytesPerCycle)
      is(index)(data_shifted := B(0, dataBusConfig.dataWidth - index * 8 bits) ## io.port_in.data ## data_tmp(0, index * 8 bits))
    default(data_shifted := io.port_in.data ## data_tmp)
  }
  when(port_in_fire) {
    when(package_pending) { //数据pending说明消耗的数据和当前port_in不是一拍数据
      data_tmp := io.port_in.data
    } otherwise {
      data_tmp := Mux(
        sel = seat_less | seat_equal,
        whenTrue = data_shifted(dataBusConfig.dataWidth, dataBusConfig.dataWidth bits), //此时port_out输出消耗数据，data_shift锁存保留高字节
        whenFalse = data_shifted(0, dataBusConfig.dataWidth bits) //数据不能输出，低位锁存
      )
    }
  }
  //port out assignment
  /** *****************************************************************************************
   * port_out.valid
   * 1.当package_pending时，此时不管port_in状态如何，port_out valid均需拉高
   * 2.当port_in.valid为高电平时
   * a.对于port_in.eop为1时，此时必须拉高port_out.valid,不管是否seat情况如何
   * b.对于非尾拍的情况，则当seat_less或者seat_equal时拉高port_out.valid
   * ***************************************************************************************** */
  io.port_out.valid := (((seat_equal || seat_less) || io.port_in.eop) & io.port_in.valid) | package_pending
  io.port_out.data := data_shifted(0, dataBusConfig.dataWidth bits)

  /** *****************************************************************************************
   * port_in.ready
   * 对于报文首拍出现package_pending时，此时输出的是上一帧报文的数据，此时一定要看port_in.ready的情况
   * 其他情况如果需要port_out输出，则需看port_out.ready的赋值，否则直接设置为True
   * ***************************************************************************************** */
  io.port_in.ready := Mux(
    sel = io.port_in.first & package_pending,
    whenTrue = io.port_out.ready,
    whenFalse = ((seat_equal || seat_less) || io.port_in.eop) ? io.port_out.ready | True
  )

  /** *****************************************************************************************
   * port_out.eop
   * 对于port_in.eop为true时，若此时有package_pending:
   * 意味着此时在输出上一帧报文的最后一批数据，需拉高eop
   * 若没有package_pending，只有当seat_less为低时(数据不会导致pending)才能拉高eop
   * ***************************************************************************************** */
  io.port_out.eop := (package_pending) || (io.port_in.eop & (~seat_less))

  /** *****************************************************************************************
   * port_out.empty
   * 在port_out.eop为低时，empty为0，若为高时：
   * 1.若此时package_pending为高，此时port_in.empty不参与，结果为dataBusConfig.bytesPerCycle-avail_num
   * 由于avail_num最高为dataBusConfig.bytesPerCycle，最高bit可抹去：0-avail_num(0,log2Up(dataBusConfig.bytesPerCycle) bits)
   * 2.若此时package_pending不为高，那么此时为dataBusConfig.bytesPerCycle-(dataBusConfig.bytesPerCycle-io.port_in.empty+avail_num)
   * 即io.port_in.empty-avail_num，此时avail_num最高位为0，可减少1bit减法
   * ***************************************************************************************** */
  io.port_out.empty := Mux(
    sel = io.port_out.eop,
    whenTrue = Mux(
      sel = package_pending,
      whenTrue = U(0, log2Up(dataBusConfig.bytesPerCycle) bits),
      whenFalse = io.port_in.empty
    ) - avail_num(0, log2Up(dataBusConfig.bytesPerCycle) bits),
    whenFalse = U(0, log2Up(dataBusConfig.bytesPerCycle) bits)
  )
  if (dataBusConfig.userWidth > 0)
    io.port_out.user := package_pending ? user_pending | io.port_in.user
}

object DataBusMergeApp extends App {
  SpinalConfig(nameWhenByFile = false).generateSystemVerilog(new DataBusMerge(DataBusConfig(512, false, 16))).printPruned()
}