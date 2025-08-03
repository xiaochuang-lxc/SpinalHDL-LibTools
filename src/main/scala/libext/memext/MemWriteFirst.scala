package libext.memext
import spinal.core._
import spinal.lib._
import spinal.core.sim._
case class MemWriteFirst[T <: Data](wordType: HardType[T], wordCount: Int,readLatency:Int) extends Component{
  require(readLatency>2) //block mem read latency=2
  val io=new Bundle{
    val wen=in Bool()
    val waddr=in UInt(log2Up(wordCount) bits)
    val wdata= in(wordType())
    val rcmd=slave(Flow(UInt(log2Up(wordCount) bits)))
    val rdata=master(Flow(wordType()))
  }
  noIoPrefix()
  /** ********************************************************************************************************
   *Mem Inst
   * *********************************************************************************************************/
  val mem=Mem(wordType,wordCount) simPublic()
  //write opreation
  mem.write(address = io.waddr,data = io.wdata,enable = io.wen)
  /** ********************************************************************************************************
   *read operation
   * *********************************************************************************************************/
  io.rdata.valid:=Delay(io.rcmd.valid,readLatency,init = False)
  val mem_rdata=mem.readSync(address = io.rcmd.payload,enable = io.rcmd.valid)
  val mem_rdata_dly=RegNext(mem_rdata) //block mem 2 read latency
  //读写冲突
  val raddr_pipe=io.rcmd.payload+:Array.fill(readLatency-1)(Reg(UInt(log2Up(wordCount) bits)))
  (raddr_pipe.tail,raddr_pipe.dropRight(1)).zipped.foreach(_:=_) //读地址打拍
  val read_write_conflict=raddr_pipe.map(raddr=>(raddr===io.waddr)&&io.wen)//读写冲突
  val rdata_sel=for(index<- 0 until readLatency-2) yield Reg(wordType())
  //前两拍处理
  val wdata_dly=Vec(Reg(wordType()),2)
  wdata_dly(0):=io.wdata
  wdata_dly(1):=wdata_dly(0)
  val read_with_write_conflict=Delay(read_write_conflict(0),2,init = False) //读写同拍
  val write_after_read_conflict=RegNext(read_write_conflict(1),False) //写滞后读一拍
  val mem_rdata_sel=Mux(
    sel = write_after_read_conflict,
    whenTrue = wdata_dly(0),
    whenFalse = read_with_write_conflict?wdata_dly(1)|mem_rdata_dly
  )
  //后续处理
  (rdata_sel,mem_rdata_sel+:rdata_sel.dropRight(1),read_write_conflict.slice(2,readLatency)).zipped.foreach((data_out,data_in,conflict)=>{
    data_out:=conflict?io.wdata|data_in
  })
  io.rdata.payload:=rdata_sel.last
}
