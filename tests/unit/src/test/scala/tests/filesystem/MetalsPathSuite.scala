package tests.filesystem

import java.net.URI
import java.nio.file.WatchEvent
import tests.BaseSuite
import scala.meta.internal.metals.filesystem.MetalsFileSystemProvider
import scala.meta.internal.metals.filesystem.MetalsPath
import scala.meta.internal.metals.filesystem.MetalsFileSystem
import scala.meta.io.RelativePath

class MetalsPathSuite extends BaseSuite {

  test("isAbsolute") {
    val absolutePath = MetalsFileSystem.metalsFS.rootPath
    val relativePath =
      MetalsPath(MetalsFileSystem.metalsFS, Array("itsAllRelative"))
    assert(absolutePath.isAbsolute)
    assert(!relativePath.isAbsolute)
  }
  test("getRoot") {
    val rootPath = MetalsFileSystem.metalsFS.rootPath
    val absolutePath = rootPath.resolve("foo").resolve("bar")
    assertEquals(absolutePath.getRoot, rootPath)
    val relativePath = MetalsPath(MetalsFileSystem.metalsFS, Array("random"))
    assertEquals(relativePath.getRoot, null)
  }
  test("getFileName") {
    val path = MetalsFileSystem.metalsFS.rootPath
      .resolve("somePath")
      .resolve("someFile.ext")
    assertEquals(
      path.getFileName,
      MetalsPath(MetalsFileSystem.metalsFS, Array("someFile.ext"))
    )
    val relativeParentPath =
      MetalsPath(MetalsFileSystem.metalsFS, Array("itsAllRelative"))
    val relativePath = relativeParentPath.resolve("someFile.ext")
    assertEquals(
      relativePath.getFileName,
      MetalsPath(MetalsFileSystem.metalsFS, Array("someFile.ext"))
    )
  }
  test("getParent") {
    val parentPath = MetalsFileSystem.metalsFS.rootPath.resolve("somePath")
    val path = parentPath.resolve("someFile.ext")
    assertEquals(path.getParent, parentPath)
    assertEquals(parentPath.getParent, MetalsFileSystem.metalsFS.rootPath)
    assertEquals(MetalsFileSystem.metalsFS.rootPath.getParent, null)
    val relativeParentPath =
      MetalsPath(MetalsFileSystem.metalsFS, Array("itsAllRelative"))
    val relativePath = relativeParentPath.resolve("someFile.ext")
    assertEquals(relativePath.getParent, relativeParentPath)
  }
  test("getNameCount") {
    val rootPath = MetalsFileSystem.metalsFS.rootPath
    val path = rootPath.resolve("someFile.ext")
    assertEquals(rootPath.getNameCount, 1)
    assertEquals(path.getNameCount, 2)
    val relativeParentPath =
      MetalsPath(MetalsFileSystem.metalsFS, Array("itsAllRelative"))
    val relativePath = relativeParentPath.resolve("someFile.ext")
    assertEquals(relativeParentPath.getNameCount, 1)
    assertEquals(relativePath.getNameCount, 2)
  }
  test("getName") {
    val filename = MetalsPath(MetalsFileSystem.metalsFS, Array("someFile.ext"))
    val midPath = MetalsPath(MetalsFileSystem.metalsFS, Array("midPath"))
    val path =
      MetalsFileSystem.metalsFS.rootPath.resolve(midPath).resolve(filename)
    assertEquals(path.getName(0), MetalsFileSystem.metalsFS.rootPath)
    assertEquals(path.getName(1), midPath)
    assertEquals(path.getName(2), filename)
    val relativeParentPath =
      MetalsPath(MetalsFileSystem.metalsFS, Array("itsAllRelative"))
    val relativePath = relativeParentPath.resolve(midPath).resolve(filename)
    assertEquals(relativePath.getName(0), relativeParentPath)
    assertEquals(relativePath.getName(1), midPath)
    assertEquals(relativePath.getName(2), filename)
  }
  test("subpath") {
    val rootPath = MetalsFileSystem.metalsFS.rootPath
    val parentPath = rootPath.resolve("somePath")
    val path = parentPath.resolve("someFile.ext")
    intercept[IllegalArgumentException] {
      assertEquals(path.subpath(0, 0), rootPath)
    }
    assertEquals(path.subpath(0, 1), rootPath)
    assertEquals(path.subpath(0, 2), parentPath)
    assertEquals(path.subpath(0, 3), path)
    assertEquals(
      path.subpath(1, 2),
      MetalsPath(MetalsFileSystem.metalsFS, Array("somePath"))
    )
    assertEquals(
      path.subpath(1, 3),
      MetalsPath(MetalsFileSystem.metalsFS, Array("somePath", "someFile.ext"))
    )
    assertEquals(
      path.subpath(2, 3),
      MetalsPath(MetalsFileSystem.metalsFS, Array("someFile.ext"))
    )
  }
  test("startsWith") {
    val parentPath = MetalsFileSystem.metalsFS.rootPath.resolve("somePath")
    val path = parentPath.resolve("someFile.ext")
    assert(path.startsWith(parentPath))
    assert(path.startsWith(path))
    assert(!parentPath.startsWith(path))
    val relativeParentPath =
      MetalsPath(MetalsFileSystem.metalsFS, Array("itsAllRelative"))
    val relativePath = relativeParentPath.resolve("someFile.ext")
    assert(relativePath.startsWith(relativeParentPath))
    assert(relativePath.startsWith(relativePath))
    assert(!relativeParentPath.startsWith(relativePath))
  }
  test("endsWith") {
    val filePath = MetalsPath(MetalsFileSystem.metalsFS, Array("someFile.ext"))
    assert(filePath.endsWith(filePath))
    val parentPath = MetalsFileSystem.metalsFS.rootPath.resolve("somePath")
    val path = parentPath.resolve(filePath)
    assert(path.endsWith(filePath))
    assert(!filePath.endsWith(path))
    val relativeParentPath =
      MetalsPath(MetalsFileSystem.metalsFS, Array("itsAllRelative"))
    val relativePath = relativeParentPath.resolve(filePath)
    assert(relativePath.endsWith(filePath))
    assert(!filePath.endsWith(relativePath))
  }
  test("normalize") {
    val rootPath = MetalsFileSystem.metalsFS.rootPath
    val path1 = rootPath.resolve("somePath1")
    val path2 = path1.resolve("somePath2")
    assertEquals(path2.normalize, path2)
    val path1RelativeA = path2.resolve("..")
    assertEquals(path1RelativeA.normalize, path1)
    val path1RelativeB =
      path2.resolve("..").resolve("somePath2").resolve("..").resolve(".")
    assertEquals(path1RelativeB.normalize, path1)
  }
  test("resolve") {
    val rootPath = MetalsFileSystem.metalsFS.rootPath
    assertEquals(
      rootPath.resolve("somePath"),
      MetalsPath(
        MetalsFileSystem.metalsFS,
        Array(MetalsFileSystemProvider.rootName, "somePath")
      )
    )
    val relativeParentPath =
      MetalsPath(MetalsFileSystem.metalsFS, Array("itsAllRelative"))
    assertEquals(
      relativeParentPath.resolve("somePath"),
      MetalsPath(MetalsFileSystem.metalsFS, Array("itsAllRelative", "somePath"))
    )
    assertEquals(relativeParentPath.resolve(rootPath), rootPath)
  }
  test("resolveSibling") {
    val rootPath = MetalsFileSystem.metalsFS.rootPath
    assertEquals(
      rootPath.resolveSibling("itsAllRelative"),
      MetalsPath(MetalsFileSystem.metalsFS, Array("itsAllRelative"))
    )
    val path = rootPath.resolve("somePath")
    val child1 = path.resolve("child1")
    assertEquals(child1.resolveSibling("child2"), path.resolve("child2"))
  }
  test("relativize") {
    val rootPath = MetalsFileSystem.metalsFS.rootPath
    val relativePath =
      MetalsPath(MetalsFileSystem.metalsFS, Array("itsAllRelative"))
    val path = rootPath.resolve(relativePath)
    assertEquals(rootPath.relativize(path), relativePath)
  }
  test("toUri") {
    val path = MetalsFileSystem.metalsFS.rootPath
      .resolve("somePath")
      .resolve("someFile.ext")
    assertEquals(
      path.toUri,
      URI.create("metalsfs:/metalsLibraries/somePath/someFile.ext")
    )
  }
  test("toAbsolutePath") {
    val rootPath = MetalsFileSystem.metalsFS.rootPath
    assertEquals(rootPath.toAbsolutePath, rootPath)
    val relativePath =
      MetalsPath(MetalsFileSystem.metalsFS, Array("itsAllRelative"))
    assertEquals(relativePath.toAbsolutePath, rootPath.resolve(relativePath))
  }
  test("toRealPath") {
    val rootPath = MetalsFileSystem.metalsFS.rootPath
    val path1 = rootPath.resolve("somePath1")
    val path2 = path1.resolve("somePath2")
    assertEquals(path2, path2.toRealPath())
    val path1RelativeA = path2.resolve("..")
    assertEquals(path1RelativeA.toRealPath(), path1)
    val path1RelativeB =
      path2.resolve("..").resolve("somePath2").resolve("..").resolve(".")
    assertEquals(path1RelativeB.toRealPath(), path1)
  }
  test("toFile") {
    intercept[UnsupportedOperationException] {
      val rootPath = MetalsFileSystem.metalsFS.rootPath
      rootPath.toFile
    }
  }

  test("register1") {
    intercept[UnsupportedOperationException] {
      val rootPath = MetalsFileSystem.metalsFS.rootPath
      rootPath.register(null, null: Array[WatchEvent.Kind[_]], null)
    }
  }
  test("register2") {
    intercept[UnsupportedOperationException] {
      val rootPath = MetalsFileSystem.metalsFS.rootPath
      rootPath.register(null, null: Array[WatchEvent.Kind[_]])
    }
  }
  test("iterator") {
    val rootPath = MetalsFileSystem.metalsFS.rootPath
    val fooPath = MetalsPath(MetalsFileSystem.metalsFS, Array("foo"))
    val barPath = MetalsPath(MetalsFileSystem.metalsFS, Array("bar"))
    val absoluteIter = rootPath.resolve(fooPath).resolve(barPath).iterator()
    assert(absoluteIter.hasNext())
    assertEquals(absoluteIter.next(), fooPath)
    assert(absoluteIter.hasNext())
    assertEquals(absoluteIter.next(), barPath)
    assert(!absoluteIter.hasNext())

    val relativeIter = fooPath.resolve(barPath).iterator()
    assert(relativeIter.hasNext())
    assertEquals(relativeIter.next(), fooPath)
    assert(relativeIter.hasNext())
    assertEquals(relativeIter.next(), barPath)
    assert(!relativeIter.hasNext())
  }
  test("compareTo") {
    val rootPath = MetalsFileSystem.metalsFS.rootPath
    val fooPath = rootPath.resolve("foo")
    val barPath = fooPath.resolve("bar")
    assertEquals(rootPath.compareTo(rootPath), 0)
    assertEquals(fooPath.compareTo(rootPath), 1)
    assertEquals(rootPath.compareTo(fooPath), -1)
    assertEquals(barPath.compareTo(fooPath), 1)
  }
  test("equals") {
    val rootPath = MetalsFileSystem.metalsFS.rootPath
    val fooPath = rootPath.resolve("foo")
    val relativePath =
      MetalsPath(MetalsFileSystem.metalsFS, Array("itsAllRelative"))
    assert(rootPath.equals(rootPath))
    assert(fooPath.equals(fooPath))
    assert(relativePath.equals(relativePath))
    assert(!rootPath.equals(relativePath))
    assert(!rootPath.equals(fooPath))
  }
  test("isJDK") {
    val rootPath = MetalsFileSystem.metalsFS.rootPath
    val jdkFolder = rootPath.resolve(MetalsFileSystem.jdkSubName)
    val workspaceJarFolder =
      rootPath.resolve(MetalsFileSystem.workspaceJarSubName)
    val sourceJarFolder = rootPath.resolve(MetalsFileSystem.sourceJarSubName)
    val jdkFile = jdkFolder.resolve("some jdk")
    val workspaceJarFile = workspaceJarFolder.resolve("some workspace jar")
    val sourceJarFile = sourceJarFolder.resolve("some source jar")
    assert(!rootPath.isJDK)
    assert(!jdkFolder.isJDK)
    assert(!workspaceJarFolder.isJDK)
    assert(!sourceJarFolder.isJDK)
    assert(jdkFile.isJDK)
    assert(!workspaceJarFile.isJDK)
    assert(!sourceJarFile.isJDK)
  }
  test("isInJar") {
    val rootPath = MetalsFileSystem.metalsFS.rootPath
    val jdkFolder = rootPath.resolve(MetalsFileSystem.jdkSubName)
    val workspaceJarFolder =
      rootPath.resolve(MetalsFileSystem.workspaceJarSubName)
    val sourceJarFolder = rootPath.resolve(MetalsFileSystem.sourceJarSubName)
    val jdkFile = jdkFolder.resolve("some jdk")
    val workspaceJarFile = workspaceJarFolder.resolve("some workspace jar")
    val sourceJarFile = sourceJarFolder.resolve("some source jar")
    assert(!rootPath.isInJar)
    assert(!jdkFolder.isInJar)
    assert(!workspaceJarFolder.isInJar)
    assert(!sourceJarFolder.isInJar)
    assert(jdkFile.isInJar)
    assert(workspaceJarFile.isInJar)
    assert(sourceJarFile.isInJar)
  }
  test("jarPath") {
    val rootPath = MetalsFileSystem.metalsFS.rootPath
    val subPath = rootPath.resolve("subName")
    val jarPath = subPath.resolve("jarName")
    val packagePath = jarPath.resolve("package")
    assertEquals(rootPath.jarPath, None)
    assertEquals(subPath.jarPath, None)
    assertEquals(jarPath.jarPath, Some(jarPath))
    assertEquals(packagePath.jarPath, Some(jarPath))
  }
  test("originalJarURI") {
    // TODO need a real jar to test on
    /*
    val filesystemJar = AbsolutePath("file://somePath/someJar.jar")
    MetalsFileSystem.metalsFS.addWorkspaceJar(filesystemJar)
    val rootPath = MetalsFileSystem.metalsFS.rootPath
    val subPath = rootPath.resolve("subName")
    val jarPath = subPath.resolve("someJar.jar")
    val packagePath = jarPath.resolve("package")
    val classPath = packagePath.resolve("class")
    assertEquals(rootPath.originalJarURI, None)
    assertEquals(subPath.originalJarURI, None)
    assertEquals(
      jarPath.originalJarURI,
      Some(URI.create("file://somePath/someJar.jar"))
    )
    assertEquals(
      packagePath.originalJarURI,
      Some(URI.create("file://somePath/someJar.jar!/package"))
    )
    assertEquals(
      classPath.originalJarURI,
      Some(URI.create("file://somePath/someJar.jar!/package/class"))
    )*/
  }
  test("jarName") {
    val rootPath = MetalsFileSystem.metalsFS.rootPath
    val subPath = rootPath.resolve("subName")
    val jarPath = subPath.resolve("jarName")
    val packagePath = jarPath.resolve("package")
    val classPath = packagePath.resolve("class")
    assertEquals(rootPath.jarName, None)
    assertEquals(subPath.jarName, None)
    assertEquals(jarPath.jarName, Some("jarName"))
    assertEquals(packagePath.jarName, Some("jarName"))
    assertEquals(classPath.jarName, Some("jarName"))
  }
  test("jarDirs") {
    val rootPath = MetalsFileSystem.metalsFS.rootPath
    val subPath = rootPath.resolve("subName")
    val jarPath = subPath.resolve("jarName")
    val packagePath = jarPath.resolve("package")
    val classPath = packagePath.resolve("class")
    assertEquals(rootPath.jarDirs, Seq.empty)
    assertEquals(subPath.jarDirs, Seq.empty)
    assertEquals(jarPath.jarDirs, Seq.empty)
    assertEquals(packagePath.jarDirs, Seq("package"))
    assertEquals(classPath.jarDirs, Seq("package", "class"))
  }
  test("classPath") {

    // TODO should classPath include module on moduled jars?  What's it used for again?
    // if so we need a jdk17 + jdk8
    val rootPath = MetalsFileSystem.metalsFS.rootPath
    val subPath = rootPath.resolve("subName")
    val jarPath = subPath.resolve("jarName")
    val packagePath = jarPath.resolve("package")
    val classPath = packagePath.resolve("class")
    assertEquals(rootPath.classPath, None)
    assertEquals(subPath.classPath, None)
    assertEquals(
      jarPath.classPath,
      Some(MetalsPath(MetalsFileSystem.metalsFS, Array.empty[String]))
    )
    assertEquals(
      packagePath.classPath,
      Some(MetalsPath(MetalsFileSystem.metalsFS, Array("package")))
    )
    assertEquals(
      classPath.classPath,
      Some(MetalsPath(MetalsFileSystem.metalsFS, Array("package", "class")))
    )

  }
  test("moduleName") {
    /*
    // TODO - need a jdk17 + jdk8 + jar17 + jar8 to test this (i.e. test with a source jar (jdk) and nonsource jar)
    val jdkWithModule = AbsolutePath("file://somePath/jdkHome17")
    val jdkWithoutModule = AbsolutePath("file://somePath/jdkHome8")
    val workspaceJarWithModule = AbsolutePath("file://somePath/nonSourceJar17")
    val workspaceJarWithoutModule = AbsolutePath("file://somePath/nonSourceJar8")
    
    MetalsFileSystem.metalsFS.addJDK(jdkWithModule)
    MetalsFileSystem.metalsFS.addJDK(jdkWithoutModule)
    MetalsFileSystem.metalsFS.addWorkspaceJar(workspaceJarWithModule)
    MetalsFileSystem.metalsFS.addWorkspaceJar(workspaceJarWithoutModule)

    val moduleJar1 = MetalsFileSystem.metalsFS.getJDK(jdkWithModule).toNIO match { case mp: MetalsPath => mp }
    val noModuleJar1 = MetalsFileSystem.metalsFS.getJDK(jdkWithoutModule).toNIO match { case mp: MetalsPath => mp }
    val moduleJar2 = MetalsFileSystem.metalsFS.getWorkspaceJar(workspaceJarWithModule).toNIO match { case mp: MetalsPath => mp }
    val noModuleJar2 = MetalsFileSystem.metalsFS.getWorkspaceJar(workspaceJarWithoutModule).toNIO match { case mp: MetalsPath => mp }

    assertEquals(moduleJar1.moduleName, Some("module-info.java"))
    assertEquals(noModuleJar1.moduleName, None)
    assertEquals(moduleJar2.moduleName, Some("module-info.class"))
    assertEquals(noModuleJar2.moduleName, None)
     */
  }
  test("fromReadOnly") {
    val readOnlyFolders = RelativePath("subName/jarName/package/class.java")
    assertEquals(
      MetalsPath.fromReadOnly(readOnlyFolders),
      MetalsPath(
        MetalsFileSystem.metalsFS,
        Array(
          MetalsFileSystemProvider.rootName,
          "subName",
          "jarName",
          "package",
          "class.java"
        )
      )
    )
  }
}
