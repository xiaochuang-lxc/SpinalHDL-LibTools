package libext.cache
import libext.cache.interface._
import spinal.core._
import spinal.lib._
import libext.memext._
case class RdataRefillWritePayload(depth:Int,config: MemPortConfig) extends Bundle{
  val rdata=Bits(config.dataWidth bits)
  val rid=UInt(log2Up(depth)+1 bits)
  val fault=Bool()
}
case class RdataRefill(depth:Int,config: MemPortConfig,readLatency:Int) extends Component{
  val io=new Bundle{
    val write_mcd=slave(Flow(RdataRefillWritePayload(depth,config)))
    val rsp=master(MemRsp(config))
    val alloc_id=master Stream(UInt(log2Up(depth)+1 bits))
    val init_done=out Bool() setAsReg() init(False)
  }
  noIoPrefix()
  /*******************************************************************************************
  Mem Inst
  *******************************************************************************************/
  val mem=Mem(MemRspPayload(config),depth)
  /*******************************************************************************************
  Flag Vec
  *******************************************************************************************/
  val flag=Vec(RegInit(True),depth)
  val rid_oh= (B(1, depth bits) |<< io.write_mcd.rid(0,log2Up(depth) bits)).asBools
  (rid_oh,flag).zipped.foreach((sel,flag_value)=>{
    when(io.init_done){
      when(io.write_mcd.valid && sel){
        flag_value:=io.write_mcd.rid.msb
      }
    }otherwise{
      flag_value.set()
    }
  })
  /*******************************************************************************************
  Write operation
  1. initial,set all mem entry to 0
  2. user cmd
  *******************************************************************************************/
  //initial process
  val counter=Counter(depth,(~io.init_done))
  when(counter.willOverflowIfInc){io.init_done.set()}
  // mem write opeation
  val write_payload=MemRspPayload(config)
  when(io.init_done){
    write_payload.rdata:=io.write_mcd.rdata
    write_payload.fault:=io.write_mcd.fault
  }otherwise{
    write_payload.rdata:=0
    write_payload.fault:=False
  }
  mem.write(address = Mux(io.init_done,io.write_mcd.rid(0,log2Up(depth) bits ),counter.value),data = write_payload,enable = (io.write_mcd.valid|(!io.init_done)))
  /*******************************************************************************************
  Mem Read Operation
  *******************************************************************************************/
  val read_cmd=Flow(UInt(log2Up(depth)bits))
  io.rsp<<mem.flowRead(read_cmd,readLatency,false,null)
  /*******************************************************************************************
  mem分配，回收指针维护
  *******************************************************************************************/
  val alloc_id,free_id=UInt(log2Up(depth)+1 bits) setAsReg() init(0)
  val full=(alloc_id.msb^free_id.msb) && (alloc_id(0,log2Up(depth) bits)===free_id(0,log2Up(depth) bits))
  val empty=alloc_id===free_id

  read_cmd.valid:= (free_id.msb===flag(free_id(0,log2Up(depth) bits))) && (!empty) //非空且最高位相等，表示此时该位已经数据回收完成
  read_cmd.payload:=free_id.resized
  when(read_cmd.fire){free_id:=free_id+1} //发出一个读指令回收加1

  io.alloc_id.valid:= !full
  io.alloc_id.payload:=alloc_id
  when(io.alloc_id.fire){
    alloc_id:=alloc_id+1
  }
}

object RdataRefillApp extends App{
  SpinalConfig(nameWhenByFile = false).generateSystemVerilog(RdataRefill(32,MemPortConfig(32,512,true),2))
  //SpinalSystemVerilog(RdataRefill(32,MemPortConfig(32,512,true),2))
}