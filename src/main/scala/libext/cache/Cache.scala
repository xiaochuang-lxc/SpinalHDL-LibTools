package libext.cache
import spinal.core._
import spinal.lib._
import libext.cache.interface._
import libext.memext._
import libext.streamExt._
import spinal.lib.pipeline._
case class CacheConfig(
                      userAddrWidth:Int,
                      userDataWidth:Int,
                      memAddrWidth:Int,
                      memDataWidth:Int,
                      cacheSize:Int,
                      wayNum:Int,
                      ramReadLatency:Int,
                      refillNum:Int,
                      writeBackNum:Int
                      ){
  require(isPow2(userDataWidth))
  require(isPow2(memDataWidth))
  require(isPow2(memDataWidth/userDataWidth))
  val userPortConfig=UserPortConfig(addrWidht = userAddrWidth, dataWidth = userDataWidth, useStrb = true)
  val memPortConfig=MemPortConfig(addrWidht = memAddrWidth, dataWidth = memDataWidth, useStrb = true)

  val cacheSizePerWay=cacheSize/wayNum //每路cache的容量大小
  val lineNumPerWay=cacheSizePerWay*8/memDataWidth //一路cache way有几行
  val lineSize=memDataWidth/8

  val tagRange = memAddrWidth-1 downto log2Up(lineNumPerWay*lineSize)
  val lineRange = tagRange.low-1 downto log2Up(lineSize)
  val refillRange= tagRange.high downto lineRange.low

  val rdataRefillCount=refillNum*2
}
case class Cache(cacheConfig: CacheConfig) extends Component{
  import cacheConfig._
  val io=new Bundle{
    val user=slave(UserPort(userPortConfig))
    val mem=master(MemPort(memPortConfig))
  }
  noIoPrefix()
  /*******************************************************************************************
  Mem Ram and Tag Context
  *******************************************************************************************/
  val data_ram=Array.fill(wayNum)(Mem(Bits(memDataWidth bits),lineNumPerWay))
  val tag_ram=Array.fill(wayNum)(Mem(TagConext(tagRange.size),lineNumPerWay))

  val data_ram_wcmd=data_ram.map(_.writePort())
  val tag_ram_wcmd=data_ram.map(_.writePort())
  val data_ram_rcmd=Array.fill(wayNum)(Flow(UInt(lineRange.size bits)))
  val tag_ram_rcmd=Array.fill(wayNum)(Flow(UInt(lineRange.size bits)))
  val cache_data=(data_ram,data_ram_rcmd).zipped.map((mem,rcmd)=>mem.flowRead(rcmd,ramReadLatency,false))
  val tag=(tag_ram,tag_ram_rcmd).zipped.map((mem,rcmd)=>mem.flowRead(rcmd,ramReadLatency,false))
  tag_ram_rcmd.foreach(_.valid.set())
  data_ram_rcmd.foreach(_.valid.set())
  /*******************************************************************************************
  refill
  *******************************************************************************************/
  val refill=Refill(cacheConfig)
  /*******************************************************************************************
  write back
  *******************************************************************************************/
  val writeback=WriteBack(cacheConfig)
  /*******************************************************************************************
  Hazard Retry Fifo
  *******************************************************************************************/
  val hazardFifo=StreamFifo(HazardContext(memAddrWidth,memDataWidth,true,log2Up(rdataRefillCount)+1),refillNum+4) //todo: afull set
  val hazard_pop= cloneOf(hazardFifo.io.pop)
  if(ramReadLatency==1) {
    hazard_pop<<hazardFifo.io.pop
  } else {
    hazard_pop<<StreamStage(hazardFifo.io.pop)
  }
  /*******************************************************************************************
  Rdata Refill Context
  *******************************************************************************************/
  val rdataRefill=RdataRefill(rdataRefillCount,memPortConfig,ramReadLatency)
  /*******************************************************************************************
  pipeline
  *******************************************************************************************/
  val wr=Stageable(Bool())                            //1:写操作 0:读操作
  val addr=Stageable(UInt(memAddrWidth bits))         //待操作地址
  val wdata=Stageable(Bits(memDataWidth bits))        //写数据
  val mask=Stageable(Bits(memDataWidth/8 bits))       //写数据掩码
  val flush=Stageable(Bool())                          //地址冲刷
  val rdata_id=Stageable(Bool())                      //读数据分配ID
  val rsp_en=Stageable(Bool())                        //是否需要返回数据
  val pip=new Pipeline{
    val stages=Array.fill(2+ramReadLatency)(new Stage())
    connect(stages)(List(Connection.M2S()))
    val fetch=stages.head
    val action=stages.last
    val judge=stages(stages.length-2)

    val fetch_op=new Area{
      import fetch._
      /*******************************************************************************************
      指令仲裁,优先级顺序:refill，hazard refill fifo,user cmd
      若refill地址与hazard地址相同,则可以将refill与hazard refill fifo指令合并处理
      *******************************************************************************************/
      val hazard_retry_en,user_proc_en=Bool()
      when(refill.refill_rsp.valid){ //如果refill有效且其地址与hazardFifo中待处理的地址相同，则可以合并处理
        hazard_retry_en:= refill.refill_rsp.info.address===hazard_pop.addr
      }otherwise{
        hazard_retry_en.set()
      }
      when(refill.refill_rsp.valid){ //refill 有指令，则优先处理
        user_proc_en.clear()
      }elsewhen(hazard_pop.valid && (!hazard_pop.wait_refilling)){ //如果hazard retry中有数据待处理，且待处理的数据并非wait_refill，则需优先处理
        user_proc_en.clear()
      }otherwise{
        when(io.user.cmd.wr){//写指令需hazardFifo中能容纳该指令
          user_proc_en:=hazardFifo.io.push.ready
        }otherwise{//如果读指令，则需hazardFifo中能容纳该指令，且alloc_id能够申请出新的id
          user_proc_en:=hazardFifo.io.push.ready && rdataRefill.io.alloc_id.valid
        }
      }
    }.setName("")

  }


}
