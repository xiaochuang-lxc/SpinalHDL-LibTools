package libext.cache.interface
import spinal.core._
import spinal.lib._

/**
 * Tag 信息存储
 * @param tagWidth tag地址宽度
 */
case class TagConext(tagWidth:Int) extends Bundle{
  val loaded=Bool() //True:数据已加载进entry。False:数据未加载进entry
  val valid=Bool()  //True:entry被占用，False：entry未被占用
  val fault=Bool()  //True表示数据有误
  val dirty=Bool()  //True:cache中数据与Mem中数据不一致
  val tag=Bits(tagWidth bits) //用于比较的地址tag位宽

  def isHazard:Bool=valid && (!loaded)

  def isReady=loaded && valid
}
