package libext.toos.report
import spinal.core._
import spinal.core.internals.DataAssignmentStatement
import spinal.lib._
import spinal.lib.com.eth._
import spinal.lib.soc.pinsec.{Pinsec, PinsecConfig}
object Test extends App{
  val config = SpinalConfig(verbose = true).dumpWave()
  val report = config.generateVerilog(new Pinsec(PinsecConfig.default))
  val reportGen=ComponentReport(report)
  reportGen.generateReport("report")
  report.printUnused()
}

object cdTest extends App{
  case class demo(clkA:ClockDomain,clkB:ClockDomain) extends Component{
    val io=new Bundle{
      val data_in=in Bool()
      val data_out=out Bool()
    }
    val a1=clkA on new Area{
      val tmp=RegNext(io.data_in)
    }
    val cc=clkB on new Area {
      val bufferCC=new BufferCC[Bool](Bool(),False,bufferDepth = Option(3))
      io.data_out<>bufferCC.io.dataOut
      a1.tmp<>bufferCC.io.dataIn
    }

  }
  val report=SpinalSystemVerilog(demo(ClockDomain.external("clkA"),ClockDomain.external("clkB")))
  println(report.toplevel.cc.bufferCC.buffers(0).hasDataAssignment)
  println(LatencyAnalysis(report.toplevel.a1.tmp,report.toplevel.cc.bufferCC.buffers(0)))
  println(report.toplevel.a1.tmp.clockDomain.clock.getName())
  report.toplevel.cc.bufferCC.buffers(0).foreachStatements(s=> s match {
    case da:DataAssignmentStatement=>{
      da.source match {
        case data: BaseType=> println(s"data:${data.getDisplayName()}\tclock:${data.clockDomain.clock.getDisplayName()}")
        case _ =>
      }
    }
    case _=>
  })
}