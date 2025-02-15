package scala.meta.internal.implementation

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

import scala.meta.internal.implementation.Supermethods.GoToSuperMethodParams
import scala.meta.internal.implementation.Supermethods.formatMethodSymbolForQuickPick
import scala.meta.internal.metals.ClientCommands
import scala.meta.internal.metals.DefinitionProvider
import scala.meta.internal.metals.JsonParser._
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.metals.MetalsLanguageClient
import scala.meta.internal.metals.MetalsQuickPickItem
import scala.meta.internal.metals.MetalsQuickPickParams
import scala.meta.internal.semanticdb.SymbolInformation
import scala.meta.io.AbsolutePath

import org.eclipse.lsp4j.ExecuteCommandParams
import org.eclipse.lsp4j.Location
import org.eclipse.lsp4j.Position

class Supermethods(
    client: MetalsLanguageClient,
    definitionProvider: DefinitionProvider,
    implementationProvider: ImplementationProvider
)(implicit
    ec: ExecutionContext
) {

  def getGoToSuperMethodCommand(
      commandParams: ExecuteCommandParams
  ): Option[ExecuteCommandParams] = {
    parseJsonParams(commandParams)
      .flatMap(getGoToSuperMethodLocation)
      .map(makeCommandParams)
  }

  def jumpToSelectedSuperMethod(
      commandParams: ExecuteCommandParams
  ): Future[Unit] = {
    def execute(
        methodSymbols: List[String],
        path: AbsolutePath
    ): Future[Unit] = {
      askUserToSelectSuperMethod(methodSymbols)
        .map(
          _.flatMap(findDefinitionLocation(_, path))
            .map(makeCommandParams)
            .foreach(client.metalsExecuteClientCommand)
        )
    }

    (for {
      params <- parseJsonParams(commandParams)
      filePath <- params.document.toAbsolutePathSafe
      methodsHierarchy <- getSuperMethodHierarchySymbols(params)
      if methodsHierarchy.nonEmpty
    } yield execute(methodsHierarchy, filePath))
      .getOrElse(Future.successful(()))
  }

  def getGoToSuperMethodLocation(
      params: GoToSuperMethodParams
  ): Option[Location] = {
    for {
      filePath <- params.document.toAbsolutePathSafe
      (symbolOcc, textDocument) <- definitionProvider.symbolOccurrence(
        filePath,
        params.position
      )
      findSymbol = implementationProvider.defaultSymbolSearch(
        filePath,
        textDocument
      )
      symbolInformation <- findSymbol(symbolOcc.symbol)
      gotoSymbol <- {
        if (symbolOcc.role.isDefinition) {
          findSuperMethodSymbol(symbolInformation)
        } else {
          Some(symbolInformation.symbol)
        }
      }
      jumpToLocation <- findDefinitionLocation(gotoSymbol, filePath)
    } yield jumpToLocation
  }

  private def askUserToSelectSuperMethod(
      methodSymbols: List[String]
  ): Future[Option[String]] = {
    client
      .metalsQuickPick(
        MetalsQuickPickParams(
          methodSymbols
            .map(symbol =>
              MetalsQuickPickItem(
                symbol,
                formatMethodSymbolForQuickPick(symbol)
              )
            )
            .asJava,
          placeHolder = "Select super method to jump to"
        )
      )
      .asScala
      .map {
        case pickResult if !pickResult.cancelled => Some(pickResult.itemId)
        case _ => None
      }
  }

  def getSuperMethodHierarchySymbols(
      params: GoToSuperMethodParams
  ): Option[List[String]] = {
    for {
      filePath <- params.document.toAbsolutePathSafe
      (symbolOcc, textDocument) <- definitionProvider.symbolOccurrence(
        filePath,
        params.position
      )
      findSymbol = implementationProvider.defaultSymbolSearch(
        filePath,
        textDocument
      )
      symbolInformation <- findSymbol(symbolOcc.symbol)
      docText = TextDocumentWithPath(textDocument, filePath)
    } yield SuperMethodProvider.getSuperMethodHierarchy(
      symbolInformation
    )
  }

  private def findSuperMethodSymbol(
      symbolInformation: SymbolInformation
  ): Option[String] = {
    SuperMethodProvider.findSuperForMethodOrField(
      symbolInformation
    )
  }

  private def findDefinitionLocation(
      symbol: String,
      source: AbsolutePath
  ): Option[Location] = {
    definitionProvider.fromSymbol(symbol, Some(source)).asScala.headOption
  }

  private def makeCommandParams(location: Location): ExecuteCommandParams = {
    new ExecuteCommandParams(
      ClientCommands.GotoLocation.id,
      List[Object](location).asJava
    )
  }

  private def parseJsonParams(
      commandParams: ExecuteCommandParams
  ): Option[GoToSuperMethodParams] = {
    for {
      args <- Option(commandParams.getArguments)
      argObject <- args.asScala.headOption
      superMethodParams <-
        argObject.toJsonObject
          .as[GoToSuperMethodParams]
          .toOption
    } yield superMethodParams
  }
}

object Supermethods {

  final case class GoToSuperMethodParams(document: String, position: Position)

  /**
   * Formats method symbol to be nicely displayed in QuickPick for user.
   * Tests visualizing how this method works are in class SuperMethodSuite.
   *
   * @param symbol  Symbol string from semanticdb which represents method.
   * @return
   */
  def formatMethodSymbolForQuickPick(symbol: String): String = {
    val replaced = symbol
      .replace("/", ".")
      .replaceAll("\\(.*\\)", "")
    replaced.substring(0, replaced.length - 1)
  }

}
