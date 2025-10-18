package libext.streamExt

import spinal.core._
import spinal.lib._

class StreamFragmentPending[T<:Data](hardType:HardType[T]) extends Component{
  val io=new Bundle{
    val pending = in (hardType)
    val port_in=slave(Stream(Fragment(hardType)))
    val port_out=master(Stream(Fragment(hardType)))
  }
  noIoPrefix()
  val up_port_enable=RegInit(True)
  io.port_out.valid:=io.port_in.valid
  io.port_out.last:= !up_port_enable
  io.port_out.fragment:=up_port_enable? io.port_in.fragment|io.pending
  io.port_in.ready:=io.port_out.ready&&up_port_enable

  when(io.port_out.fire){
    when(io.port_in.last && (!io.port_out.last)){
      up_port_enable.clear()
    }elsewhen(io.port_out.last){
      up_port_enable.set()
    }
  }
}

object StreamFragmentPending {
  def apply[T <: Data](port_in:Stream[Fragment[T]],pending:T): Stream[Fragment[T]] = {
    val streamFragmentPending=new StreamFragmentPending(port_in.fragment)
    streamFragmentPending.io.port_in<<port_in
    streamFragmentPending.io.pending:=pending
    streamFragmentPending.io.port_out
  }
}

case class StreamFragmentPendingDemo() extends Component{
  val data_in=slave(Stream(Fragment(Bits(32 bits))))
  val data_out=master(Stream(Fragment(Bits(32 bits))))
  val pending=in Bits(32 bits)
  data_out<<StreamFragmentPending(data_in,pending)
}

object TestDemoApp extends App{
  SpinalConfig(inlineConditionalExpression = true,nameWhenByFile=false,genLineComments = true).generateSystemVerilog(StreamFragmentPendingDemo())
}