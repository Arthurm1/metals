package tests.filesystem

import java.net.URI
import tests.BaseSuite
import scala.meta.internal.metals.filesystem.MetalsFileSystemProvider
import scala.meta.internal.metals.filesystem.MetalsPath
import scala.meta.internal.metals.filesystem.MetalsFileSystem
import scala.meta.internal.metals.urlstreamhandler.MetalsURLStreamHandlerFactory
import scala.meta.internal.metals.JdkSources
import scala.meta.internal.mtags.MtagsEnrichments._
import java.nio.file.Files

class MetalsFileSystemSuite extends BaseSuite {

  test("root") {
    val rootPath1 =
      MetalsPath(MetalsFileSystem.metalsFS, MetalsFileSystemProvider.rootNames)
    val rootPath2 = MetalsFileSystem.metalsFS.getPath("/metalsLibraries")
    assertEquals(true, rootPath1.isAbsolute)
    assertEquals(rootPath1.toUri(), MetalsFileSystemProvider.rootURI)
    assertEquals(rootPath1.getRoot(), rootPath1)
    assertEquals(rootPath2, rootPath1)
  }

  test("navigate") {
    val rootPath =
      MetalsPath(MetalsFileSystem.metalsFS, MetalsFileSystemProvider.rootNames)
    val fooPath = rootPath.resolve("Foo")
    assertEquals(fooPath.toUri(), URI.create("metalsfs:/metalsLibraries/Foo"))
    val barPath = fooPath.resolve("Bar")
    assertEquals(
      barPath.toUri(),
      URI.create("metalsfs:/metalsLibraries/Foo/Bar")
    )
    assertEquals(barPath.getParent(), fooPath)
    assertEquals(fooPath.getParent(), rootPath)
  }

  test("getPath") {
    val rootPath = MetalsFileSystem.metalsFS.getPath("/metalsLibraries")
    val foo1Path = MetalsFileSystem.metalsFS.getPath("/metalsLibraries", "Foo")
    val bar1Path =
      MetalsFileSystem.metalsFS.getPath("/metalsLibraries", "Foo", "Bar")
    val foo2Path = MetalsFileSystem.metalsFS.getPath("/metalsLibraries/Foo")
    val bar2Path = MetalsFileSystem.metalsFS.getPath("/metalsLibraries/Foo/Bar")
    val bar3Path =
      MetalsFileSystem.metalsFS.getPath("/metalsLibraries", "Foo/Bar")
    val bar4Path =
      MetalsFileSystem.metalsFS.getPath("//metalsLibraries/Foo/Bar")
    val bar5Path =
      MetalsFileSystem.metalsFS.getPath("///metalsLibraries/Foo/Bar")
    assertEquals(rootPath.toUri(), URI.create("metalsfs:/metalsLibraries"))
    assertEquals(foo1Path.toUri(), URI.create("metalsfs:/metalsLibraries/Foo"))
    assertEquals(
      bar1Path.toUri(),
      URI.create("metalsfs:/metalsLibraries/Foo/Bar")
    )
    assertEquals(foo2Path, foo1Path)
    assertEquals(bar2Path, bar1Path)
    assertEquals(bar3Path, bar1Path)
    assertEquals(bar4Path, bar1Path)
    assertEquals(bar5Path, bar1Path)
    assertEquals(true, foo1Path.isAbsolute)
    assertEquals(true, bar1Path.isAbsolute)
  }

  test("getOriginalJarURL") {
    MetalsURLStreamHandlerFactory.register
    val jarPath = JdkSources().right.get
    val metalsPath = MetalsFileSystem.metalsFS.addJDK(jarPath)
    val metalsURI = metalsPath.toNIO.toUri

    val metalsURLWithSchemeSpecificPart = new URI(
      metalsURI.getScheme,
      metalsURI.getPath,
      "runtime"
    ).toURL()
    val originalURLWithSchemeSpecificPart =
      MetalsFileSystem.metalsFS.getOriginalJarURL(
        metalsURLWithSchemeSpecificPart
      )

    assertEquals(
      originalURLWithSchemeSpecificPart.map(_.getRef()),
      Some(metalsURLWithSchemeSpecificPart.getRef)
    )

    val metalsURLWithHost = new URI(
      metalsURI.getScheme,
      metalsURI.getHost,
      metalsURI.getPath,
      "runtime"
    ).toURL()
    val originalURLWithHost =
      MetalsFileSystem.metalsFS.getOriginalJarURL(metalsURLWithHost)

    assertEquals(
      originalURLWithHost.map(_.getRef()),
      Some(metalsURLWithHost.getRef)
    )
  }

  test("getOriginalJarURI") {
    // tested in MetalsPathSuite#originalJarURI
  }

  test("getMetalsJarPath") {
    val jarPath = JdkSources().right.get
    val fullJarPath =
      s"jar:${jarPath.toNIO.toUri()}!/java/util/String.java".toAbsolutePath
    val metalsPath = MetalsFileSystem.metalsFS.addJDK(jarPath)
    val metalsJarPath =
      MetalsFileSystem.metalsFS.getMetalsJarPath(fullJarPath.toURI)

    val fullMetalsPath = Some(metalsPath.resolve("java/util/String.java"))

    assertEquals(fullMetalsPath, metalsJarPath)
  }

  test("accessLibrary") {
    val jarPath = JdkSources().right.get
    val metalsPath = MetalsFileSystem.metalsFS.addJDK(jarPath)
    val fileCount = metalsPath.toNIO match {
      case mp: MetalsPath =>
        MetalsFileSystem.metalsFS.accessLibrary(
          mp,
          path => Files.list(path).asScala.toList.size
        )
    }
    assert(fileCount.nonEmpty)
    fileCount.foreach(count => assert(count > 0))
  }

  test("isMetalsFileSystem") {
    val jarPath = JdkSources().right.get
    val metalsPath = MetalsFileSystem.metalsFS.addJDK(jarPath)
    assert(MetalsFileSystem.isMetalsFileSystem(metalsPath.toNIO))
    assert(!MetalsFileSystem.isMetalsFileSystem(jarPath.toNIO))
  }
}
