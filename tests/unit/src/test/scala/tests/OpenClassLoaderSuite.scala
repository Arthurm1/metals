package tests

import java.net.URI

import scala.meta.internal.metals.JdkSources
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.metals.filesystem.MetalsFileSystem
import scala.meta.internal.metals.urlstreamhandler.MetalsURLStreamHandlerFactory
import scala.meta.internal.mtags.OpenClassLoader

class OpenClassLoaderSuite extends BaseSuite {

  test("jdk") {
    MetalsURLStreamHandlerFactory.register
    val jdkPath = JdkSources().right.get
    val jarPath = URI.create(s"${jdkPath.toNIO.toUri}").toAbsolutePath
    val metalsPath = MetalsFileSystem.metalsFS.addJDK(jarPath)

    val jarOCL = new OpenClassLoader()
    val metalsOCL = new OpenClassLoader()

    jarOCL.addEntry(jarPath)
    metalsOCL.addEntry(metalsPath)

    val classWithoutModule = "java/net/URI.java"
    val classWithModule = s"java.base/$classWithoutModule"

    val jarResolveClassWithoutModule = jarOCL.resolve(classWithoutModule)
    val metalsResolveClassWithoutModule = metalsOCL.resolve(classWithoutModule)

    assertEquals(metalsResolveClassWithoutModule, jarResolveClassWithoutModule)

    val jarResolveClassWithModule = jarOCL.resolve(classWithModule)
    val metalsResolveClassWithModule = metalsOCL.resolve(classWithModule)

    assertEquals(metalsResolveClassWithModule, jarResolveClassWithModule)
  }
}
