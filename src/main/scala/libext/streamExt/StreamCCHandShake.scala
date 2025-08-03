package libext.streamExt

import spinal.core._
import spinal.lib._

case class StreamCCHandShake[T <: Data](input: Stream[T], output: Stream[T], inputClock: ClockDomain, outputClock: ClockDomain, bufferDepth: Int, withOutputPipe: Boolean) extends Area {
  val pop_ack = Bool()
  val push_area = inputClock on new Area {
    val ack = BufferCC(pop_ack, False, bufferDepth)
    val task_pending = RegInit(False) setWhen (input.fire) clearWhen (ack.rise(False))
    input.ready := !task_pending & !ack
    val data = RegNextWhen(input.payload, input.fire)
  }
  val pop_area = outputClock on new Area {
    val stream = cloneOf(input)
    val req = BufferCC(push_area.task_pending, False, bufferDepth)
    val ack = RegInit(False) setWhen (stream.fire) clearWhen (!req)
    pop_ack := ack
    val r_valid = RegInit(False) setWhen (req.rise(False)) clearWhen (stream.fire)
    stream.payload := push_area.data addTag (crossClockDomain)
    stream.valid := r_valid
    output << (if (withOutputPipe) stream.m2sPipe() else stream)
  }
}
