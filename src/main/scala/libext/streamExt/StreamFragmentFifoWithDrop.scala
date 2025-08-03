package libext.streamExt

import spinal.core._
import spinal.lib._

case class PayloadLinked[T <: Data](dataType: HardType[T]) extends Bundle {
  val data = dataType()
  val error = Bool()
}

case class StreamFragmentFifoWithDrop[T <: Data](dataType: HardType[T], depth: Int, withPopPipe: Boolean) extends Component {
  val io = new Bundle {
    val pkg_in = slave(Flow(Fragment(PayloadLinked(dataType))))
    val pkg_out = master(Stream(Fragment(dataType)))
    val pkg_drop_inc = out Bool()
  }
  noIoPrefix()
  require(isPow2(depth))
  /** *****************************************************************************************
   * Memory Inst
   * ***************************************************************************************** */
  val ptrWidth = log2Up(depth + 1)
  val ram = Mem(Bits(dataType.getBitsWidth + 1 bits), depth)
  val write_ptr = Reg(UInt(ptrWidth bits)) init (0)
  val write_ptr_tmp = cloneOf(write_ptr) setAsReg() init (0)
  val read_ptr = Reg(UInt(ptrWidth bits)) init (0)
  val read_ptr_next = cloneOf(read_ptr)
  val empty = write_ptr === read_ptr
  val full = (write_ptr_tmp.msb ^ read_ptr.msb) & (write_ptr_tmp(ptrWidth - 2 downto 0) === read_ptr(ptrWidth - 2 downto 0))
  val pop_full = (write_ptr.msb ^ read_ptr.msb) & (write_ptr(ptrWidth - 2 downto 0) === read_ptr(ptrWidth - 2 downto 0))
  /** *****************************************************************************************
   * Memory Write Operation
   * ***************************************************************************************** */
  val pkg_wen_flag = RegInit(True) //整包报文写使能有效标志。对于非尾拍报文
  when(io.pkg_in.valid) {
    when(io.pkg_in.last) {
      pkg_wen_flag.set()
    } elsewhen (full) {
      pkg_wen_flag.clear()
    }
  }
  val wen = io.pkg_in.valid & (!full) & pkg_wen_flag & (!io.pkg_in.error) //memory 写使能
  //写地址累加计算
  when(io.pkg_in.valid) {
    when((!full) & pkg_wen_flag & (!io.pkg_in.error)) {
      write_ptr_tmp := write_ptr_tmp + 1
    } otherwise {
      write_ptr_tmp := write_ptr
    }
  }
  //mem 真实写地址更新
  when(wen & io.pkg_in.last & !io.pkg_in.error) {
    write_ptr := write_ptr_tmp + 1
  }
  val wdata = io.pkg_in.data ## io.pkg_in.last
  ram.write(write_ptr_tmp(ptrWidth - 2 downto 0), wdata, wen)

  /** *****************************************************************************************
   * 丢包标志处理
   * ***************************************************************************************** */
  io.pkg_drop_inc := RegNext(io.pkg_in.valid & io.pkg_in.last & (full || (!pkg_wen_flag) || io.pkg_in.error), False)
  /** *****************************************************************************************
   * Memory Read Operation
   * ***************************************************************************************** */
  val pop_port = cloneOf(io.pkg_out)
  //读指针更新
  read_ptr_next := read_ptr + U(pop_port.fire)
  read_ptr := read_ptr_next
  //read operation
  val mem_rdata = ram.readSync(read_ptr_next(ptrWidth - 2 downto 0))
  pop_port.last := mem_rdata.lsb
  pop_port.fragment assignFromBits (mem_rdata(1, dataType.getBitsWidth bits))
  pop_port.valid := !empty & !(RegNext(read_ptr_next(ptrWidth - 2 downto 0) === write_ptr(ptrWidth - 2 downto 0), False) & !pop_full)
  /** *****************************************************************************************
   * with pop pipe
   * ***************************************************************************************** */
  val pipe = (withPopPipe) generate new Area {
    io.pkg_out << StreamStage(pop_port)
  }.setName("")
  val normal = (!withPopPipe) generate new Area {
    io.pkg_out << pop_port
  }
}

object StreamFragmentFifoWithDropApp extends App {
  SpinalSystemVerilog(StreamFragmentFifoWithDrop(UInt(8 bits), 16, true))
}