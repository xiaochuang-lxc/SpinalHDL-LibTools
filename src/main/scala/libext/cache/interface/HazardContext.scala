package libext.cache.interface
import spinal.core._
import spinal.lib._
case class HazardContext(addrWidth:Int,dataWidth:Int,useStrb:Boolean=true,rspIdWidth:Int) extends Bundle{
  val wr=Bool()
  val addr=UInt(addrWidth bits)
  val wdata=UInt(dataWidth bits)
  val strb=Bits(dataWidth/8 bits)
  val flush=Bool()
  val rsp_id=UInt(rspIdWidth bits)
  val wait_refilling=Bool() //表示该指令处于等待refill完成
}
