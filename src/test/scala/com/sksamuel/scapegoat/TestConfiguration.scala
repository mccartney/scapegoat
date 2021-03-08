package com.sksamuel.scapegoat

object TestConfiguration {

  def configuration = Configuration(
    dataDir = None,
    disabledInspections = List(),
    enabledInspections = List(),
    ignoredFiles = List(),
    consoleOutput = Some(false),
    verbose = Some(false),
    reports = Reports(
      disableXML = true,
      disableHTML = true,
      disableScalastyleXML = true,
      disableMarkdown = true
    ),
    customInspectors = Seq(),
    sourcePrefix = Some("src/main/scala"),
    minimalLevel = Some(Levels.Info),
    overrideLevels = Map.empty[String, Level]
  )
}
