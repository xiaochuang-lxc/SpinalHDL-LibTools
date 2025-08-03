package libext.toos.report

import spinal.core._
import spinal.core.internals.InitAssignmentStatement

import scala.collection.mutable.{ArrayBuffer, Map}
import java.io.{File, PrintWriter}
import java.nio.file.{Files, Paths}

case class ComponentReport[T <: Component](spinalReport: SpinalReport[T]) {
  val component=spinalReport.toplevel

  val unUsedSignalInfo=Map[String,ArrayBuffer[String]]()
  unUsedSignalClassify()

  def generateReport(path: String) = {
    //generate direction
    val targetDir = Paths.get(path)
    if (!Files.exists(targetDir))
      Files.createDirectories(targetDir)
    generateCss(targetDir = path)
    //generate all component information
    generateComponentInfo(module = component, targetDir = path)
    //generate index.html
    generateIndexHtml(path)


  }

  def generateIndexHtml(targetDir: String) = {
    val targetFileName = s"${targetDir}/index.html"
    val fileHdl = new File(targetFileName)
    val writer = new PrintWriter(fileHdl, "UTF-8")
    writer.write(
      s"""
         |<!DOCTYPE html>
         |<html>
         |<head>
         |  <title>${component.getClass.getName}</title>
         |  <style>
         |    .tree li {
         |      list-style-type: none;
         |      position: relative;
         |      padding-left: 0ch;
         |    }
         |    .tree li > span {
         |      cursor: pointer;
         |      display: inline-block;
         |    }
         |    .tree li > span::before {
         |      content: "+";
         |      display: inline-block;
         |      width: 0.5ch;
         |      margin-right: 0.5em;
         |    }
         |    .tree li.open > span::before {
         |      content: "-";
         |    }
         |    .tree a {
         |      cursor: pointer;
         |    }
         |    .tree li > ul {
         |      margin-left: 1ch;
         |      padding-left: 0.5ch;
         |      display: none;
         |    }
         |    .tree li.open > ul {
         |      display: block;
         |    }
         |  </style>
         |</head>
         |<body>
         |  <div class="tree">
         |    <ul>
         |""".stripMargin)
    generateModuleIndexInfo(component, writer)
    writer.write(
      """
        |    </ul>
        |  </div>
        |  <script>
        |    function toggle(element) {
        |      var parent = element.parentNode;
        |      if (parent.classList.contains('open')) {
        |        parent.classList.remove('open');
        |      } else {
        |        parent.classList.add('open');
        |      }
        |    }
        |  </script>
        |</body>
        |</html>
        |""".stripMargin)
    writer.close()
  }

  def generateModuleIndexInfo(module: Component, writer: PrintWriter): Boolean = {
    val context = new StringBuilder()
    writer.write(
      s"""
         |      <li>
         |        <span onclick="toggle(this)">${module.getDisplayName()}(${module.getClass.getName})</span>
         |        <a href="${module.getPath("_")}.html" target="_blank">&#x2794;</a >
         |""".stripMargin)
    if (!module.children.isEmpty) {
      writer.write("""<ul>""")
    }
    for (child <- module.children) {
      generateModuleIndexInfo(child, writer)
    }
    if (module.children.isEmpty) {
      writer.write(
        """
          |      </li>
          |""".stripMargin)
    } else {
      writer.write(
        """
          |      </li>
          |</ul>
          |""".stripMargin)
    }
    true
  }


  def generateCss(targetDir: String) = {
    val targetFileName = s"${targetDir}/style.css"
    val fileHdl = new File(targetFileName)
    val writer = new PrintWriter(fileHdl, "UTF-8")
    writer.write(
      """
        |.container {
        |    text-align: center;
        |    text-align: left;
        |  }
        |
        |  .title, .content {
        |    margin-left: 20px;
        |    text-align: left;
        |  }
        |
        |  .content {
        |    margin-top: 20px;
        |  }
        |
        |  table {
        |    border-collapse: collapse;
        |    border: 1px solid black;
        |    margin: 0 auto;
        |  }
        |
        |  th, td {
        |    padding: 5px;
        |    border: 1px solid black;
        |  }
        |
        |""".stripMargin)
    writer.close()
  }

  /**
   * 每个模块生成一个html报告
   * 包含IO端口、Unused Signal、时钟信息
   * @param module
   * @param targetDir
   * @return
   */
  def generateComponentInfo(module: Component, targetDir: String): Boolean = {
    val targetFileName = s"${targetDir}/${module.getPath("_")}.html"
    val fileHdl = new File(targetFileName)
    val writer = new PrintWriter(fileHdl, "UTF-8")
    val moduleInfo = ModuleInfo(module)
    val htmlContext = new StringBuilder()
    htmlContext.append(
      s"""
         |<!DOCTYPE html>
         |<html>
         |<head>
         |  <title>${module.getPath(".")}</title>
         |  <link rel="stylesheet" type="text/css" href="style.css">
         |</head>
         |<body>
         |  <div class="container">
         |<h1>Component Info</h1>
         |<hr>
         |<div class="content">
         |  <p>Component:${module.getClass.getSimpleName}</p >
         |  <p>Lib location:${module.getClass.getName}</p >
         |  <p>InstanceName:${module.getDisplayName()}</p >
         |  <p>Hierarchy:${module.getPath(".")}</p >
         |</div>
         |  </div>
         |</body>
         |</html>
         |""".stripMargin)
    //generate io information
    htmlContext.append(moduleInfo.generateIoInformation())
    //generate Clock Information
    htmlContext.append(moduleInfo.generateClockDomainInformation())
    htmlContext.append(
      """
        |  </table>
        |</div>
        |""".stripMargin)
    //generate unusedSignal
    htmlContext.append(generateUnusedSignalReport(module.getPath()))
    writer.write(htmlContext.toString())
    writer.close()
    for (child <- module.children) {
      generateComponentInfo(child, targetDir)
    }
    true
  }

  def generateUnusedSignalReport(hierarchy:String):String={
    val context=new StringBuilder()
    if (unUsedSignalInfo.contains(hierarchy)){
      context.append(
        """
          |<h1>UnUsedSignal</h1>
          |<hr>
          |<div class="content">
          |  <table>
          |""".stripMargin)
     for (signal<- unUsedSignalInfo(hierarchy)){
       context.append(
         s"""
            |    <tr>
            |      <td>${signal}</td>
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

  def unUsedSignalClassify()={
    for(signal <- spinalReport.unusedSignals){
      val signalPath=signal.getComponents().map(_.getDisplayName())
      var key=signalPath.reduce(_+"/"+_)
      var value=signal.getDisplayName()
      if (signal.dirString()!=""){
        if(signal.isOutput){
          key=signalPath.init.reduce(_+"/"+_)
          value=s"${signalPath.last}.${signal.getDisplayName()}"
        }
      }
      unUsedSignalInfo.getOrElseUpdate(key,ArrayBuffer[String]()).append(value)
    }
  }

}
