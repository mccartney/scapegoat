package com.sksamuel.scapegoat

import java.io.File

case class Reports(
  disableXML: Boolean,
  disableHTML: Boolean,
  disableScalastyleXML: Boolean,
  disableMarkdown: Boolean
)

case class Configuration(
  dataDir: Option[File],
  disabledInspections: List[String],
  enabledInspections: List[String],
  ignoredFiles: List[String],
  consoleOutput: Option[Boolean],
  verbose: Option[Boolean],
  reports: Reports,
  customInspectors: Seq[Inspection],
  sourcePrefix: Option[String],
  minimalLevel: Option[Level],
  overrideLevels: Map[String, Level]
) {
  def isVerbose: Boolean = verbose.getOrElse(false)
  def getMinimalLevel: Level = minimalLevel.getOrElse(Levels.Info)
  def shouldOutputToConsole: Boolean = consoleOutput.getOrElse(true)
  def getSourcePrefix: String = sourcePrefix.getOrElse("src/main/scala/")
}

object Configuration {

  def fromPluginOptions(options: List[String]): Configuration = {
    def fromProperty[T](propertyName: String, defaultValue: T)(fn: String => T): T = {
      options.find(_.startsWith(propertyName + ":")) match {
        case Some(property) =>
          val justTheValue = property.drop(propertyName.length + 1)
          fn(justTheValue)
        case None =>
          defaultValue
      }
    }

    val disabledInspections =
      fromProperty("disabledInspections", defaultValue = List.empty[String])(_.split(':').toList)
    val enabledInspections =
      fromProperty("enabledInspections", defaultValue = List.empty[String])(_.split(':').toList)
    val consoleOutput =
      fromProperty[Option[Boolean]]("consoleOutput", defaultValue = None)(x => Some(x.toBoolean))
    val ignoredFiles =
      fromProperty("ignoredFiles", defaultValue = List.empty[String])(_.split(':').toList)
    val verbose = fromProperty[Option[Boolean]]("verbose", defaultValue = None)(x => Some(x.toBoolean))

    val customInspectors = fromProperty("customInspectors", defaultValue = Seq.empty[Inspection]) {
      _.split(':').toSeq
        .map(inspection => Class.forName(inspection).getConstructor().newInstance().asInstanceOf[Inspection])
    }
    val enabledReports = fromProperty("reports", defaultValue = Seq("all"))(_.split(':').toSeq)
    val disableXML = !(enabledReports.contains("xml") || enabledReports.contains("all"))
    val disableHTML = !(enabledReports.contains("html") || enabledReports.contains("all"))
    val disableScalastyleXML =
      !(enabledReports.contains("scalastyle") || enabledReports.contains("all"))
    val disableMarkdown = !(enabledReports.contains("markdown") || enabledReports.contains("all"))

    val levelOverridesByInspectionSimpleName =
      fromProperty("overrideLevels", defaultValue = Map.empty[String, Level]) {
        _.split(":").map { nameLevel =>
          nameLevel.split("=") match {
            case Array(insp, level) => insp -> Levels.fromName(level)
            case _ =>
              throw new IllegalArgumentException(
                s"Malformed argument to 'overrideLevels': '$nameLevel'. " +
                "Expecting 'name=level' where 'name' is the simple name of " +
                "an inspection and 'level' is the simple name of a " +
                "com.sksamuel.scapegoat.Level constant, e.g. 'Warning'."
              )
          }
        }.toMap
      }
    val sourcePrefix = fromProperty[Option[String]]("sourcePrefix", defaultValue = None)(x => Some(x))
    val minimalLevel = fromProperty[Option[Level]]("minimalLevel", defaultValue = None) { value =>
      Some(Levels.fromName(value))
    }
    val dataDir = fromProperty[Option[File]](
      "dataDir",
      defaultValue = None
    ) { value =>
      Some(new File(value))
    }

    Configuration(
      dataDir = dataDir,
      disabledInspections = disabledInspections,
      enabledInspections = enabledInspections,
      ignoredFiles = ignoredFiles,
      consoleOutput = consoleOutput,
      verbose = verbose,
      reports = Reports(
        disableXML = disableXML,
        disableHTML = disableHTML,
        disableScalastyleXML = disableScalastyleXML,
        disableMarkdown = disableMarkdown
      ),
      customInspectors = customInspectors,
      sourcePrefix = sourcePrefix,
      minimalLevel = minimalLevel,
      overrideLevels = levelOverridesByInspectionSimpleName
    )
  }

  val optionsHelp: String = {
    Seq(
      "-P:scapegoat:dataDir:<pathtodatadir>                 where the report should be written",
      "-P:scapegoat:disabledInspections:<listofinspections> colon separated list of disabled inspections (defaults to none)",
      "-P:scapegoat:enabledInspections:<listofinspections>  colon separated list of enabled inspections (defaults to all)",
      "-P:scapegoat:customInspectors:<listofinspections>    colon separated list of custom inspections",
      "-P:scapegoat:ignoredFiles:<patterns>                 colon separated list of regexes to match ",
      "                                                     files to ignore.",
      "-P:scapegoat:verbose:<boolean>                       enable/disable verbose console messages",
      "-P:scapegoat:consoleOutput:<boolean>                 enable/disable console report output",
      "-P:scapegoat:reports:<reports>                       colon separated list of reports to generate.",
      "                                                     Valid options are `xml', `html', `scalastyle', 'markdown',",
      "                                                     or `all'. Use `none' to disable reports.",
      "-P:scapegoat:overrideLevels:<levels>                 override the built in warning levels, e.g. to",
      "                                                     downgrade a Error to a Warning.",
      "                                                     <levels> should be a colon separated list of name=level",
      "                                                     settings, where 'name' is the simple name of an inspection",
      "                                                     and 'level' is the simple name of a",
      "                                                     com.sksamuel.scapegoat.Level constant, e.g. 'Warning'.",
      "                                                     You can use 'all' for inspection name to operate on all inspections.",
      "-P:scapegoat:sourcePrefix:<prefix>                   overrides source prefix if it differs from src/main/scala",
      "                                                     for ex., in Play applications where sources are in app/ folder",
      "-P:scapegoat:minimalLevel:<level>                    provides minimal level of triggered inspections,",
      "                                                     that will be shown in a report.",
      "                                                     'level' is the simple name of a",
      "                                                     com.sksamuel.scapegoat.Level constant, e.g. 'Warning'."
    ).mkString("\n")
  }
}
