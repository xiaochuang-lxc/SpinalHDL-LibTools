package libext.cache
import spinal.core._
import spinal.lib._
import libext.streamExt.StreamStage
import libext.cache.interface._
case class Refill(cacheConfig: CacheConfig) extends Area{
  val refill_cmd=RefillContext(RefillContextConfig(addrWidth = cacheConfig.refillRange.size, wayNumWidth = log2Up(cacheConfig.wayNum)))
  val read_cmd=Stream(UInt(cacheConfig.refillRange.size bits))
  val rdata_in=MemRsp(cacheConfig.memPortConfig)
  val refill_rsp=Flow(new Bundle{
    val data=MemRspPayload(cacheConfig.memPortConfig)
    val info=RefillContextPayload(refill_cmd.config)
  })
  /*******************************************************************************************
  refill cmd fifo
  address,way info fifo
  *******************************************************************************************/
  val refillFifo=StreamFifo(UInt(cacheConfig.refillRange.size bits),cacheConfig.refillNum)
  val wayInfoFifo=StreamFifo(RefillContextPayload(refill_cmd.config),cacheConfig.refillNum)
  /*******************************************************************************************
  指令压入
  *******************************************************************************************/
  val refill_event=Event
  refill_event arbitrationFrom refill_cmd
  val(cmd_event,info_event)=StreamFork2(refill_event,true)
  refillFifo.io.push arbitrationFrom(cmd_event)
  wayInfoFifo.io.push arbitrationFrom(info_event)
  refillFifo.io.push.payload:=refill_cmd.address
  wayInfoFifo.io.push.payload:=refill_cmd.payload
  /*******************************************************************************************
  读指令发射
  *******************************************************************************************/
  if (cacheConfig.ramReadLatency==2){
    StreamStage(refillFifo.io.pop,read_cmd)
  }else{
    read_cmd<<refillFifo.io.pop
  }
  /*******************************************************************************************
  数据返回
  *******************************************************************************************/
  val way_info=RefillContext(RefillContextConfig(addrWidth = cacheConfig.refillRange.size, wayNumWidth = log2Up(cacheConfig.wayNum)))
  if (cacheConfig.ramReadLatency==2){
    StreamStage(wayInfoFifo.io.pop,way_info)
  }else{
    way_info<<wayInfoFifo.io.pop
  }
  way_info.ready:=rdata_in.valid
  refill_rsp.valid:=rdata_in.valid
  refill_rsp.data:=rdata_in.payload
  refill_rsp.info:=way_info.payload
}
