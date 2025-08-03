package libext.bitmap

import spinal.core._
import spinal.lib._
import libext.memext.MemWriteFirst
class BitMap(bitmapNum:Int,ramDataWidth:Int) extends Component{
  require(isPow2(ramDataWidth))
  require(bitmapNum%ramDataWidth==0)
  val io=new Bundle{
    val cmpl=slave(Flow(UInt(log2Up(bitmapNum) bits)))
    val avail=master(Stream(UInt(log2Up(bitmapNum) bits)))
  }
  noIoPrefix()
  /** ********************************************************************************************************
   * bit map Mem Inst(极限情况可考虑采用双端口RAM进行读端口的复制)
   * *********************************************************************************************************/
  val bitmapMem=MemWriteFirst(Bits(ramDataWidth bits),bitmapNum/ramDataWidth,3)
  /** ********************************************************************************************************
   * ping-pang data struct
   * *********************************************************************************************************/
  val sel_primary=RegInit(True)
  val primary_bitmap=Reg(Bits(ramDataWidth bits)) init(0)
  val backup_bitmap=Reg(Bits(ramDataWidth bits)) init(0)
  val bit_index=UInt(log2Up(ramDataWidth) bits) setAsReg() init(0)
  val primary_addr=UInt(log2Up(bitmapNum/ramDataWidth) bits) setAsReg() init(0)
  val backup_addr=UInt(log2Up(bitmapNum/ramDataWidth) bits) setAsReg() init(1)
  val cmpl_addr=io.cmpl.payload(log2Up(bitmapNum)-1 downto log2Up(ramDataWidth))
  val cmpl_index=io.cmpl.payload(0,log2Up(ramDataWidth) bits)
  val cmpl_index_oh=UIntToOh(cmpl_index)
  val cmpl_hit_primary=cmpl_addr===primary_addr
  val cmpl_hit_backup=cmpl_addr===backup_addr
  /** ********************************************************************************************************
   * output port assignment
   * *********************************************************************************************************/
  val avail_tmp=Stream(UInt(log2Up(bitmapNum) bits))
  avail_tmp.payload:=sel_primary ?(primary_addr@@bit_index)|(backup_addr@@bit_index)
  avail_tmp.valid:=sel_primary ?primary_bitmap(bit_index)|backup_bitmap(bit_index)
  bit_index:=bit_index+U(avail_tmp.fire)
  when(avail_tmp.fire && bit_index.andR){
    sel_primary:= !sel_primary
  }
  /** ********************************************************************************************************
   * ping-pang assignment
   * *********************************************************************************************************/
  val primary_read_event,backup_read_event=Stream(UInt(log2Up(bitmapNum/ramDataWidth) bits))
  val backup_read_finished=Delay(backup_read_event.fire,3,init = False)
  val primary_read_finished=Delay(primary_read_event.fire,3,init = False)
  //addr assignment
  when(avail_tmp.fire && bit_index.andR && sel_primary){
    primary_addr:=(backup_addr===(bitmapNum/ramDataWidth-1))?U(0,log2Up(bitmapNum/ramDataWidth) bits)|(backup_addr+1)
  }
  when(avail_tmp.fire && bit_index.andR && (!sel_primary)){
    backup_addr:= (primary_addr===(bitmapNum/ramDataWidth-1))?U(0,log2Up(bitmapNum/ramDataWidth) bits)|(primary_addr+1)
  }
  // bitmap set
  when(avail_tmp.fire && bit_index.andR && sel_primary){
    primary_bitmap.clearAll()
  }otherwise{
    primary_bitmap:=primary_bitmap|(Repeat(io.cmpl.valid&&cmpl_hit_primary,ramDataWidth)&cmpl_index_oh)|(Repeat(primary_read_finished,ramDataWidth)&bitmapMem.io.rdata.payload)
  }
  when(avail_tmp.fire && bit_index.andR && (!sel_primary)){
    backup_bitmap.clearAll()
  }otherwise{
    backup_bitmap:=backup_bitmap|(Repeat(io.cmpl.valid&&cmpl_hit_backup,ramDataWidth)&cmpl_index_oh)|(Repeat(backup_read_finished,ramDataWidth)&bitmapMem.io.rdata.payload)
  }
  backup_read_event.valid.setAsReg()
  when(avail_tmp.fire && bit_index.andR && (!sel_primary)){ //read ram request generation
    backup_read_event.valid.set()
  }elsewhen(backup_read_event.fire){
    backup_read_event.valid.clear()
  }
  primary_read_event.valid.setAsReg()
  when(avail_tmp.fire && bit_index.andR && sel_primary){ //read ram request generation
    primary_read_event.valid.set()
  }elsewhen(primary_read_event.fire){
    primary_read_event.valid.clear()
  }
  primary_read_event.payload:=primary_addr
  backup_read_event.payload:=backup_addr
  /** ********************************************************************************************************
   * bitmap clear request generation
   * *********************************************************************************************************/
  val clear_addr=UInt(log2Up(bitmapNum/ramDataWidth) bits) setAsReg() init(0)
  val clear_event=Stream(UInt(log2Up(bitmapNum/ramDataWidth) bits))
  clear_event.valid:=clear_addr=/=(Mux(sel_primary,primary_addr,backup_addr))
  when(clear_event.fire){
    when(clear_addr===(bitmapNum/ramDataWidth-1)){
      clear_addr.clearAll()
    }otherwise{
      clear_addr:=clear_addr+1
    }
  }
  clear_event.payload:=clear_addr
  /** ********************************************************************************************************
   * ram read(read after write)
   * cmpl read first
   * primary backup read second
   * bitmap clear read last
   * *********************************************************************************************************/
  val cmpl_event=Stream(UInt(log2Up(bitmapNum/ramDataWidth) bits))
  cmpl_event.valid:=io.cmpl.valid && (!cmpl_hit_backup) && (!cmpl_hit_primary)
  cmpl_event.payload:=cmpl_addr
  val rd_cmd=StreamArbiterFactory().lowerFirst.transactionLock.on(Seq(cmpl_event,primary_read_event,backup_read_event,clear_event))
  rd_cmd.toFlow<>bitmapMem.io.rcmd
  /** ********************************************************************************************************
   * ram write(read after write)
   * cmpl_write
   * backup
   * bitmap clear read last
   * *********************************************************************************************************/
    val cmpl_valid_mem_read=Delay(cmpl_event.fire,3,init = False)
  val clear_event_read_finished=Delay(clear_event.fire,3,init = False)
  val cmpl_index_oh_update=Delay(cmpl_index_oh,3)
  bitmapMem.io.wen:=cmpl_valid_mem_read || clear_event_read_finished
  bitmapMem.io.waddr:=Delay(bitmapMem.io.rcmd.payload,3)
  when(cmpl_valid_mem_read){
    bitmapMem.io.wdata:=cmpl_index_oh_update|bitmapMem.io.rdata.payload
  }otherwise{//clear bitmap
    bitmapMem.io.wdata:=0
  }
  avail_tmp.pipelined(true,true)<>io.avail
}
object BitMapApp extends App{
  SpinalSystemVerilog(new BitMap(1024,16)).printPruned()
}