package libext.cache
import spinal.core._
import spinal.lib._
case class WriteBackContext(addressWidth:Int,dataWdith:Int) extends Bundle {
  val address=UInt(addressWidth bits)
  val wdata=Bits(dataWdith bits)
}
case class WriteBack(cacheConfig: CacheConfig) {
  import cacheConfig._

  val writeback_req=Stream(WriteBackContext(refillRange.size,memDataWidth))
  val writeback_invalid=Flow(UInt(log2Up(writeBackNum) bits))
  val write_cmd=Stream(WriteBackContext(refillRange.size,memDataWidth))

  val serach_address=UInt(refillRange.size bits)
  val writeback_hit=Flow(UInt(log2Up(writeBackNum) bits))

  /*******************************************************************************************
  Write Back Context
  *******************************************************************************************/
  val writeback_entry=Vec(WriteBackContext(refillRange.size,memDataWidth),writeBackNum)
  val writeback_valid=Vec(RegInit(False),writeBackNum)
  val push_ptr,pop_ptr=Reg(UInt(log2Up(writeBackNum)+1 bits)) init(0)
  val full=(push_ptr.msb^pop_ptr.msb) && (push_ptr(0,log2Up(writeBackNum) bits)===pop_ptr(0,log2Up(writeBackNum) bits))
  val empty=push_ptr===pop_ptr
  /*******************************************************************************************
  write cmd proc
  *******************************************************************************************/
  when(writeback_invalid.valid){ //如果invalid指令有效，且invalid entry与当前窗口相同，则该指令不发出
    write_cmd.valid:= (!empty) && (writeback_invalid.payload=/=pop_ptr(0,log2Up(writeBackNum) bits))
  }otherwise {
    write_cmd.valid:= !empty
  }
  write_cmd.payload:=writeback_entry(pop_ptr(0,log2Up(writeBackNum) bits))
  //pop_ptr process
  when((writeback_invalid.valid && (writeback_invalid.payload===pop_ptr(0,log2Up(writeBackNum) bits)) && (!empty))||write_cmd.fire){
    pop_ptr:=pop_ptr+1
  }
  /*******************************************************************************************
  write back req process
  *******************************************************************************************/
  writeback_req.ready:= !full
  val push_ptr_oh=UIntToOh(push_ptr(0,log2Up(writeBackNum) bits))
  for(index<-0 until writeBackNum) {
    when(writeback_req.fire & push_ptr_oh(index)) {
      writeback_entry(index) := writeback_req.payload
    }
  }
  when(writeback_req.fire){push_ptr:=push_ptr+1}
  /*******************************************************************************************
  writeback valid process
  影响参数：write_cmd,writeback_invalid,writeback_req。
  writeback_valid与writeback_invalid不可能操作到同一个entry上，这种情况下依旧清零
  *******************************************************************************************/
  val invalid_oh=UIntToOh(writeback_invalid.payload)
  val pop_ptr_oh=UIntToOh(pop_ptr(0,log2Up(writeBackNum) bits))
  for(index<-0 until writeBackNum){
    when((write_cmd.fire&&pop_ptr_oh(index))||(writeback_invalid.valid && invalid_oh(index))){ //对应entry发出读指令或者被invalid时清零
      writeback_valid(index).clear()
    }elsewhen(writeback_req.fire & push_ptr_oh(index)){ //对应entry有压入请求时设置为1
      writeback_valid(index).set()
    }
  }
  /*******************************************************************************************
  判定指定地址是否命中
  *******************************************************************************************/
  val writeback_hit_oh=(writeback_entry,writeback_valid).zipped.map((writebackCtxt,valid)=>valid && (writebackCtxt.address===serach_address))
  writeback_hit.valid:=writeback_hit_oh.orR
  writeback_hit.payload:=OHToUInt(writeback_hit_oh)

}
