package libext.streamExt

import spinal.core._
import spinal.lib._

/**
 * src->dst 打拍，与SpinalHDL自带库m2s不同，payload直接打拍，不带任何条件判断
 *
 * @param dataType
 * @tparam T
 */
class StreamStage[T <: Data](dataType: HardType[T]) extends Component {
  val io = new Bundle {
    val src = slave(Stream(dataType))
    val dst = master(Stream(dataType))
  }
  noIoPrefix()
  val payload_dly = RegNext(io.src.payload)
  val fire_dly = RegNext(io.src.fire, False)
  val valid_hold = Reg(Bool(), False)
  val payload_hold = RegNextWhen(payload_dly, fire_dly & (!io.dst.ready))
  when(io.dst.ready) {
    valid_hold.clear()
  } elsewhen (fire_dly & (!io.dst.ready)) {
    valid_hold.set()
  }
  io.dst.valid := valid_hold || fire_dly
  io.dst.payload := Mux(valid_hold, payload_hold, payload_dly)
  io.src.ready := io.dst.ready || (!io.dst.valid)
}

object StreamStage {
  def apply[T <: Data](src: Stream[T], dst: Stream[T]) = {
    val streamPipe = new StreamStage(src.payload)
    streamPipe.io.src << src
    dst << streamPipe.io.dst
  }

  def apply[T <: Data](src: Stream[T]): Stream[T] = {
    val streamPipe = new StreamStage(src.payload)
    streamPipe.io.src << src
    streamPipe.io.dst
  }
}