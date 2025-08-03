package libext.cache.interface
import spinal.core._
import spinal.core._
import spinal.lib._

case class RefillContextConfig(addrWidth:Int,wayNumWidth:Int) {

}

case class RefillContextPayload(config: RefillContextConfig) extends Bundle {
  val address=UInt(config.addrWidth bits)
  val way=UInt(config.wayNumWidth bits)
}

class RefillContext(config: RefillContextConfig) extends Stream(RefillContextPayload(config)) {

}

object RefillContext {
  def rename(bus: RefillContext) = {
    bus.flatten.foreach((bt) => {
      bt.setName(bt.getName().replace("payload_", ""))
      if (bt.getName().startsWith("io_")) bt.setName(bt.getName().replaceFirst("io_", ""))
    })
  }

  def apply(config: RefillContextConfig): RefillContext = {
    val bus = new RefillContext(config)
    if (Component.current == bus.component)
      bus.component.addPrePopTask(() => {
        rename(bus)
      })
    else
      rename(bus)
    bus

  }

}
