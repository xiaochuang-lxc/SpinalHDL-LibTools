package libext.toos.report

import spinal.core._
import spinal.lib.tools._

import scala.collection.mutable.ArrayBuffer

case class ModuleInfo(component: Component) {
  val moduleAnalyzer = new ModuleAnalyzer(component)

  /**
   * get Component Io Bundle
   *
   * @param component
   * @return
   */
  private def getIoBundle(component: Component): Data = {
    try {
      val m = component.getClass.getMethod("io")
      m.invoke(component).asInstanceOf[Data]
    } catch {
      case _: Throwable => null
    }
  }

  def getIos() = {
    val ioInformation = ArrayBuffer[Data]()
    val allIo = component.getAllIo.clone()
    val io = getIoBundle(component)
    if (io == null) {
      ioInformation ++= allIo
    } else {
      for ((name, ele) <- io.asInstanceOf[Bundle].elements) {
        ioInformation += ele
        ele.flattenForeach { signal => allIo.remove(signal) }
      }
      ioInformation ++= allIo
    }
    ioInformation.toArray
  }

  def generateIoHtmlTable(headerName: String, signalList: Array[Data]): String = {
    val context = new StringBuilder()
    context.append(
      s"""
         |<h2>$headerName</h2>
         |<div class="content">
         |  <table>
         |    <tr>
         |      <th>SignalName</th>
         |      <th>Type</th>
         |      <th>Direction</th>
         |      <th>Width</th>
         |    </tr>
         |""".stripMargin)
    for (signal <- signalList) {
      context.append(
        s"""
           |    <tr>
           |      <td>${signal.getDisplayName()}</td>
           |      <td>${signal.getClass.getSimpleName}</td>
           |      <td>${signal.dirString()}</td>
           |      <td>${signal.getBitsWidth}</td>
           |    </tr>
           |""".stripMargin)
    }
    context.append(
      """
        |  </table>
        |</div>
        |""".stripMargin)
    context.toString()
  }

  def generateIoInformation(): String = {
    val context = new StringBuilder()
    context.append(
      """
        |<h1>IO</h1>
        |<hr>
        |""".stripMargin)
    val ioList = getIos()
    val ioTmpList = ArrayBuffer[Data]()
    for (io <- ioList) {
      if (io.isInstanceOf[Bundle]) {
        context.append(generateIoHtmlTable(headerName = io.getClass.getName, io.flatten.toArray))
      } else {
        ioTmpList.append(io)
      }
    }
    context.append(generateIoHtmlTable(headerName = "Dispersed signal", ioTmpList.toArray))
    context.toString()
  }

  def generateClockDomainInformation(): String = {
    val context = new StringBuilder()
    context.append(
      """
        |<h1>ClockDomain</h1>
        |<hr>
        |""".stripMargin)
    //get all clockdomain
    val clockDomainSet = moduleAnalyzer.getClocks
    for (clockDomain <- clockDomainSet) {
      context.append(
        s"""
           |<h2>${clockDomain.clock.getName()}</h2>
           |<div class="content">
           |  <table>
           |""".stripMargin)
      context.append(
        s"""
           |    <tr>
           |      <td>clockName</td>
           |      <td>${clockDomain.clock.getDisplayName()}</td>
           |    </tr>
           |    <tr>
           |      <td>clockEdge</td>
           |      <td>${if (clockDomain.config.clockEdge == RISING) "RISING" else "FALLING"}</td>
           |    </tr>
           |""".stripMargin)
      if (clockDomain.hasClockEnableSignal) {
        context.append(
          s"""
             |    <tr>
             |      <td>clockName</td>
             |      <td>${clockDomain.clockEnable.getDisplayName()}</td>
             |    </tr>
             |    <tr>
             |      <td>clockEnableActiveLevel</td>
             |      <td>${if (clockDomain.config.clockEnableActiveLevel == HIGH) "HIGH" else "LOW"}</td>
             |    </tr>
             |""".stripMargin)
      }
      if (clockDomain.hasResetSignal) {
        context.append(
          s"""
             |    <tr>
             |      <td>resetName</td>
             |      <td>${clockDomain.reset.getDisplayName()}</td>
             |    </tr>
             |    <tr>
             |      <td>resetKind</td>
             |      <td>${if (clockDomain.config.resetKind == SYNC) "SYNC" else "ASYNC"}</td>
             |    </tr>
             |    <tr>
             |      <td>resetActiveLevel</td>
             |      <td>${if (clockDomain.config.resetActiveLevel == HIGH) "HIGH" else "LOW"}</td>
             |    </tr>
             |""".stripMargin)
      }
      if (clockDomain.hasSoftResetSignal) {
        context.append(
          s"""
             |    <tr>
             |      <td>softResetName</td>
             |      <td>${clockDomain.softReset.getDisplayName()}</td>
             |    </tr>
             |    <tr>
             |      <td>softResetName</td>
             |      <td>${clockDomain.softReset.getDisplayName()}</td>
             |    </tr>
             |    <tr>
             |      <td>softResetActiveLevel</td>
             |      <td>${if (clockDomain.config.softResetActiveLevel == HIGH) "HIGH" else "LOW"}</td>
             |    </tr>
             |""".stripMargin)
      }
      if (!clockDomain.frequency.isInstanceOf[UnknownFrequency]) {
        context.append(
          s"""
             |    <tr>
             |      <td>frequency</td>
             |      <td>${clockDomain.frequency.getValue}</td>
             |    </tr>
             |""".stripMargin)
      }
      if (!clockDomain.clockEnableDivisionRate.isInstanceOf[ClockDomain.UnknownDivisionRate]) {
        context.append(
          s"""
             |    <tr>
             |      <td>DivisionRate</td>
             |      <td>${clockDomain.clockEnableDivisionRate.getValue}</td>
             |    </tr>
             |""".stripMargin)
      }
      context.append(
        """
          |  </table>
          |</div>
          |""".stripMargin)
    }
    context.toString()
  }
}
