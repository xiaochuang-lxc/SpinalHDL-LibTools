package libext.memext

import spinal.core._
import spinal.lib._

case class MemoryWriteCmd[T <: Data](addreseWidth: Int, payloadType: HardType[T]) extends Bundle {

  val address = UInt(addreseWidth bits)
  val wdata = payloadType()

  this.component.addPrePopTask(() =>
    this.flattenForeach((bt) => {
      bt.setName(bt.getName().replace("_payload_", "_"))
    }
    )
  )
}


class MemExpand[T <: Data](mem: Mem[T]) {

  /**
   * 输入读之令，返回读数据
   *
   * @param read_cmd   读指令，paylood为待读取地址
   * @param writeFirst 为true表示写优先，T0发起读请求，如果TO cycle有写过相同地址，则返回最新的写数据。T0+1返回数据，
   * @param write_cmd  写指令。当使能writeFirst时不能为null
   * @return 读返回数据FLow流
   */

  def flowReadSync(read_cmd: Flow[UInt], writeFirst: Boolean = false, write_cmd: Flow[MemoryWriteCmd[T]] = null): Flow[T] = new Composite(mem) {
    val ret = Flow(mem.wordType)
    ret.valid := RegNext(read_cmd.valid, False)

    val rdata = mem.readSync(read_cmd.payload, read_cmd.valid)

    val normal = (!writeFirst) generate new Area {
      ret.payload := rdata
    }.setName("")

    val ahead = (writeFirst) generate new Area {
      val conflict = RegNext(read_cmd.valid & write_cmd.valid & (write_cmd.address === read_cmd.payload), False)
      val wdata_tmp = RegNext(write_cmd.wdata)
      ret.payload := Mux(sel = conflict, whenTrue = wdata_tmp, whenFalse = rdata)
    }.setName("")
  }.ret

  /**
   * 输入读之令，返回读数据，附带每个指令需要的附加信息
   *
   * @param read_cmd   读指令
   * @param linkedData 指令附带数据
   * @param writeFirst 为true表示写优先，T0发起读请求，如果TO cycle有写过相同地址，则返回最新的写数据。T0+1返回数据，
   * @param write_cmd  写指令。当使能writeFirst时不能为null
   * @tparam T2
   * @return 读返回Flow流并附带附加信息
   */
  def flowReadWithLinkedData[T2 <: Data](read_cmd: Flow[UInt], linkedData: T2, writeFirst: Boolean = false, write_cmd: Flow[MemoryWriteCmd[T]] = null): Flow[ReadRetLinked[T, T2]] = new Composite(mem) {
    val ret = Flow(new ReadRetLinked(mem.wordType, linkedData))
    val read_no_link = flowReadSync(read_cmd, writeFirst, write_cmd)
    ret.valid := read_no_link.valid
    ret.value := read_no_link.payload
    ret.linked := RegNext(linkedData)
  }.ret

  /**
   * 输入读指令，返回读数据，读延迟有2拍delay
   *
   * @param read_cmd   读指令，payload为读返回数据
   * @param writeFirst 为true表示写优先,T0发起读请求，如果T0，T1 cycle有写相同地址，则返回最新的写数据，T2读数据返回
   * @param write_cmd  写指令，当使能writeFirst时不能为null
   * @return 读返回数据flow流
   */
  def flowReadSyncEnhanced(read_cmd: Flow[UInt], writeFirst: Boolean = false, write_cmd: Flow[MemoryWriteCmd[T]] = null): Flow[T] = new Composite(mem) {
    val ret = Flow(mem.wordType)
    val rdata_t1 = flowReadSync(read_cmd, false, write_cmd)

    val normal = (!writeFirst) generate new Area {
      ret << rdata_t1.stage()
    }.setName("")

    val ahead = (writeFirst) generate new Area {
      ret.valid := RegNext(rdata_t1.valid, False)

      val read_with_write_conflict = RegNext(read_cmd.valid & write_cmd.valid & (write_cmd.address === read_cmd.payload), False)
      val read_cmd_dly = read_cmd.stage()
      val read_before_write_conflict = read_cmd_dly.valid & write_cmd.valid & (write_cmd.address === read_cmd_dly.payload)
      val wdata_t0_dly = RegNext(write_cmd.wdata)
      val rdata_t1_sel = cloneOf(ret.payload)
      when(read_before_write_conflict) {
        rdata_t1_sel := write_cmd.wdata
      } elsewhen (read_with_write_conflict) {
        rdata_t1_sel := wdata_t0_dly
      } otherwise {
        rdata_t1_sel := rdata_t1.payload
      }
      ret.payload := RegNext(rdata_t1_sel)
    }.setName("")
  }.ret

  /**
   * 输入读指令,返回读数据，附带每个指令需要的附加信息，两拍延迟
   *
   * @param read_cmd   读指令，payload为读返回数据
   * @param linkedData 指令附带数据
   * @param writeFirst 为true表示写优先,T0发起读请求，如果T0，T1 cycle有写相同地址，则返回最新的写数据，T2读数据返回
   * @param write_cmd  写指令，当使能writeFirst时不能为null
   * @tparam T2
   * @return 读返回Flow流并附带附加信息
   */
  def flowReadWithLinkedDataEnhanced[T2 <: Data](read_cmd: Flow[UInt], linkedData: T2, writeFirst: Boolean = false, write_cmd: Flow[MemoryWriteCmd[T]] = null): Flow[ReadRetLinked[T, T2]] = new Composite(mem) {
    val ret = Flow(new ReadRetLinked(mem.wordType, linkedData))
    val read_no_link = flowReadSyncEnhanced(read_cmd, writeFirst, write_cmd)
    ret.valid := read_no_link.valid
    ret.value := read_no_link.payload
    ret.linked := Delay(linkedData, 2)
  }.ret

  /**
   * mem flow 读接口
   *
   * @param read_cmd   读指令
   * @param read_delay 读延迟，仅支持1/2
   * @param writeFirst 为true表示写优先
   * @param write_cmd  写指令，当使能writeFirst时不能为null
   * @return 读返回Flow流
   */
  def flowRead(read_cmd: Flow[UInt], read_delay: Int, writeFirst: Boolean = false, write_cmd: Flow[MemoryWriteCmd[T]] = null): Flow[T] = {
    assert(read_delay == 1 || read_delay == 2, s"read_delay only support 1 or 2 cycle")
    if (read_delay == 1) {
      flowReadSync(read_cmd, writeFirst, write_cmd)
    } else {
      flowReadSyncEnhanced(read_cmd, writeFirst, write_cmd)
    }
  }

  /**
   * Mem flow读接口，读指令可附带信息
   *
   * @param read_cmd   读指令
   * @param linkedData 指令附带数据
   * @param read_delay 读延迟，仅支持1/2
   * @param writeFirst 为true表示写优先
   * @param write_cmd  写指令，当使能writeFirst时不能为null
   * @tparam T2
   * @return 读返回Flow流
   */
  def flowReadWithLinked[T2 <: Data](read_cmd: Flow[UInt], linkedData: T2, read_delay: Int, writeFirst: Boolean = false, write_cmd: Flow[MemoryWriteCmd[T]] = null): Flow[ReadRetLinked[T, T2]] = {
    assert(read_delay == 1 || read_delay == 2, s"read_delay only support 1 or 2 cycle")
    if (read_delay == 1) {
      flowReadWithLinkedData(read_cmd, linkedData, writeFirst, write_cmd)
    } else {
      flowReadWithLinkedDataEnhanced(read_cmd, linkedData, writeFirst, write_cmd)
    }
  }

  /**
   * Memory Stream读接口
   *
   * @param read_cmd   读指令
   * @param writeFirst 为true表示写优先，T0发起读请求，如果T0 cycle有写过相同的地址，则返回最新的写数据。T0+1返回数据
   * @param write_cmd  写指令，当write first时需附带该指令
   * @return
   */
  def streamReadSync(read_cmd: Stream[UInt], writeFirst: Boolean = false, write_cmd: Flow[MemoryWriteCmd[T]] = null): Stream[T] = new Composite(mem) {
    val ret = Stream(mem.wordType)
    val ren = Bool()
    val rdata = mem.readSync(read_cmd.payload, ren)
    val rvalid = RegInit(False)
    //若当前cycle ret.ready为高，此时正好pipe，read_cmd.ready可为高，若此时ret无任务，说明ret空闲，read_cmd可以进行指令处理
    read_cmd.ready := ret.ready || (!ret.valid)

    /** *****************************************************************************************
     * 当read_cmd.ready为高电平，此时允许read_cmd指令消耗，若此时read_cmd.valid,则下一拍将会ret输出
     * 若read_cmd.ready为低电平，那么意味这要么此时ret处于valid&(!ready)状态，上一个指令还没有消耗，rvalid保持即可
     * ***************************************************************************************** */
    when(read_cmd.ready) {
      rvalid := read_cmd.valid
    }

    ret.valid := rvalid

    val normal = (!writeFirst) generate new Area {
      ren := read_cmd.ready //read_cmd能否fire,取决于ready是否为高，故用ready来作为是否读取mem
      ret.payload := rdata
    }.setName("")

    val ahead = (writeFirst) generate new Area {
      val conflict = RegNext(write_cmd.valid & read_cmd.ready & (write_cmd.address === read_cmd.payload), False)
      ren := read_cmd.ready || conflict //加上conflict为高时恰好此时ret.valid为高电平，而此时ret.ready为低点评，保障 下一cycle仍能输出正确结果
      val write_tmp = RegNext(write_cmd.wdata)
      ret.payload := Mux(sel = conflict, whenTrue = write_tmp, whenFalse = rdata)
    }.setName("")
  }.ret

  /**
   * 带linked的Stream读接口
   *
   * @param read_cmd   读指令
   * @param linkedData 附带信息
   * @param writeFirst 为true表示写优先，T0发起读请求，如果T0 cycle有写过相同的地址，则返回最新的写数据。T0+1返回数据
   * @param write_cmd  写指令，当write first时需附带该指令
   * @tparam T2
   * @return
   */
  def streamReadWithLinkedData[T2 <: Data](read_cmd: Stream[UInt], linkedData: T2, writeFirst: Boolean = false, write_cmd: Flow[MemoryWriteCmd[T]] = null): Stream[ReadRetLinked[T, T2]] = new Composite(mem) {
    val ret = Stream(new ReadRetLinked(mem.wordType, linkedData))
    val ret_no_link = streamReadSync(read_cmd, writeFirst, write_cmd)
    val ret_linkdata = RegNextWhen(linkedData, read_cmd.ready)
    ret.linked := ret_linkdata
    ret.value := ret_no_link.payload
    ret arbitrationFrom ret_no_link
  }.ret


  def streamReadSyncEnhanced(read_cmd: Stream[UInt], writeFirst: Boolean = false, write_cmd: Flow[MemoryWriteCmd[T]] = null): Stream[T] = new Composite(mem) {

    import libext.streamExt.StreamStage

    val ret = Stream(mem.wordType)
    val ret_read_first = streamReadSync(read_cmd, false, write_cmd)

    val normal = (!writeFirst) generate new Area {
      StreamStage(ret_read_first, ret)
    }.setName("")

    val ahead = (writeFirst) generate new Area {
      val read_and_write_conflict = RegNextWhen(write_cmd.valid & read_cmd.valid & (write_cmd.address === read_cmd.payload), read_cmd.ready, False) //在读mem同时出现读写指令冲突
      val read_dly = RegNext(read_cmd.fire, False) //读指令延迟
      val read_addr_dly = RegNext(read_cmd.payload)
      val write_tmp = RegNextWhen(write_cmd.wdata, read_cmd.ready)
      val read_before_write_conflict = write_cmd.valid & read_dly & (read_addr_dly === write_cmd.address) //读发生在写的头一拍
      val read_before_write_conflict_hold = RegInit(False)
      val write_tmp_hold = RegNextWhen(write_cmd.wdata, (!ret_read_first.ready) & (!read_before_write_conflict_hold))
      when(ret_read_first.ready) {
        read_before_write_conflict_hold.clear()
      } otherwise {
        read_before_write_conflict_hold := read_before_write_conflict || read_before_write_conflict_hold
      }
      val totalBits = 1 + 1 + write_tmp.getBitsWidth * 3
      val read_tmp = Stream(Bits(totalBits bits))
      read_tmp arbitrationFrom ret_read_first
      read_tmp.payload := (read_before_write_conflict || read_before_write_conflict_hold) ## read_and_write_conflict ## Mux(read_before_write_conflict_hold, write_tmp_hold.asBits, write_cmd.wdata.asBits) ## write_tmp ## ret_read_first.payload
      val ret_tmp = Stream(Bits(totalBits bits))
      StreamStage(read_tmp, ret_tmp)
      ret arbitrationFrom ret_tmp
      val read_data_tmp = ret_tmp.payload.asBits(totalBits - 3 downto 0).subdivideIn(write_tmp.getBitsWidth bits)
      when(ret_tmp.payload.msb) { //read before write conflict
        ret.payload.assignFromBits(read_data_tmp(2))
      } elsewhen (ret_tmp.payload(totalBits - 2)) { //read and write conflict
        ret.payload.assignFromBits(read_data_tmp(1))
      } otherwise {
        ret.payload.assignFromBits(read_data_tmp(0))
      }
    }.setName("")
  }.ret

  /**
   * mem flow 读接口
   *
   * @param read_cmd   读指令
   * @param read_delay 读延迟，仅支持1/2
   * @param writeFirst 为true表示写优先
   * @param write_cmd  写指令，当使能writeFirst时不能为null
   * @return 读返回Flow流
   */
  def streamRead(read_cmd: Stream[UInt], read_delay: Int, writeFirst: Boolean = false, write_cmd: Flow[MemoryWriteCmd[T]] = null): Stream[T] = {
    assert(read_delay == 1 || read_delay == 2, s"read_delay only support 1 or 2 cycle")
    if (read_delay == 1) {
      streamReadSync(read_cmd,writeFirst,write_cmd)
    } else {
      streamReadSyncEnhanced(read_cmd,writeFirst,write_cmd)
    }
  }


  def streamReadWithLinkedDataEnhanced[T2 <: Data](read_cmd: Stream[UInt], linkedData: T2, writeFirst: Boolean = false, write_cmd: Flow[MemoryWriteCmd[T]] = null): Stream[ReadRetLinked[T, T2]] = new Composite(mem) {

    import libext.streamExt.StreamStage

    val ret = Stream(new ReadRetLinked(mem.wordType, linkedData))
    val ret_read_first = streamReadWithLinkedData(read_cmd, linkedData, false, write_cmd)

    val normal = (!writeFirst) generate new Area {
      StreamStage(ret_read_first, ret)
    }.setName("")

    val ahead = (writeFirst) generate new Area {
      val read_and_write_conflict = RegNextWhen(write_cmd.valid & read_cmd.valid & (write_cmd.address === read_cmd.payload), read_cmd.ready, False) //在读mem同时出现读写指令冲突
      val read_dly = RegNext(read_cmd.fire, False)
      val read_addr_dly = RegNext(read_cmd.payload)
      val write_tmp = RegNextWhen(write_cmd.wdata, read_cmd.ready)
      val read_before_write_conflict = write_cmd.valid & read_dly & (read_addr_dly === write_cmd.address) //读发生在写的头一拍
      val read_before_write_conflict_hold = RegInit(False)
      val write_tmp_hold = RegNextWhen(write_cmd.wdata, (!ret_read_first.ready) & (!read_before_write_conflict_hold))
      when(ret_read_first.ready) {
        read_before_write_conflict_hold.clear()
      } otherwise {
        read_before_write_conflict_hold := read_before_write_conflict || read_before_write_conflict_hold
      }
      val totalBits = 1 + 1 + write_tmp.getBitsWidth * 3 + ret_read_first.linked.getBitsWidth
      val read_tmp = Stream(Bits(totalBits bits))
      read_tmp arbitrationFrom ret_read_first
      read_tmp.payload := (read_before_write_conflict || read_before_write_conflict_hold) ## read_and_write_conflict ## ret_read_first.payload.asBits ## Mux(read_before_write_conflict_hold, write_tmp_hold.asBits, write_cmd.wdata.asBits) ## write_tmp
      val ret_tmp = Stream(Bits(totalBits bits))
      StreamStage(read_tmp, ret_tmp)
      ret arbitrationFrom ret_tmp
      val read_data_tmp = ret_tmp.payload.asBits(totalBits - 3 - ret_read_first.linked.getBitsWidth downto 0).subdivideIn(write_tmp.getBitsWidth bits)
      when(ret_tmp.payload.msb) { //read before write conflict
        ret.payload.assignFromBits(read_data_tmp(1))
      } elsewhen (ret_tmp.payload(totalBits - 2)) { //read and write conflict
        ret.payload.assignFromBits(read_data_tmp(0))
      } otherwise {
        ret.payload.assignFromBits(read_data_tmp(2))
      }
      ret.linked.assignFromBits(ret_tmp.payload.asBits(totalBits - 3 downto write_tmp.getBitsWidth * 3))
    }.setName("")
  }.ret
}
