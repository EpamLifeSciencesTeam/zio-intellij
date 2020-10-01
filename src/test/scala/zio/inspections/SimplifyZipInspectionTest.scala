package zio.inspections

import com.intellij.testFramework.EditorTestUtil.{SELECTION_END_TAG => END, SELECTION_START_TAG => START}
import zio.intellij.inspections.simplifications.SimplifyZipInspection

abstract class ZipInspectionTest(s: String) extends ZSimplifyInspectionTest[SimplifyZipInspection] {
  override protected val hint = s"Replace with $s"
}

class SimplifyFlatmapWithZipRightTest extends ZipInspectionTest(".zipRight") {

  def test_flatMap_to_zipRight(): Unit = {
    z(s"""ZIO.succeed("Remedios Varo").${START}flatMap(_ => x)$END""").assertHighlighted()
    val text   = z("""ZIO.succeed("Remedios Varo").flatMap(_ => x)""")
    val result = z("""ZIO.succeed("Remedios Varo").zipRight(x)""")
    testQuickFixes(text, result, hint)
  }

  def test_blockflatMap_to_zipRight(): Unit = {
    z {
      s"""ZIO.succeed("Remedios Varo").${START}flatMap { _ =>
         |  x
         |  x
         |  x
         |}$END""".stripMargin
    }.assertHighlighted()

    val text = z {
      """ZIO.succeed("Remedios Varo").flatMap { _ =>
        |  x
        |  x
        |  x
        |}""".stripMargin
    }
    val result = z {
      """ZIO.succeed("Remedios Varo").zipRight {
        | x
        | x
        | x
        |}""".stripMargin
    }
    testQuickFixes(text, result, hint)
  }

  def test_flatMap_not_discarding_should_not_highlight(): Unit =
    z(s"""ZIO.succeed("Tarsila do Amaral").${START}flatMap(x => x)$END""").assertNotHighlighted()

}

class SimplifyTapWithZipLeftTest extends ZipInspectionTest(".zipLeft") {

  def test_tap_to_zipLeft(): Unit = {
    z(s"""ZIO.succeed("Remedios Varo").${START}tap(_ => x)$END""").assertHighlighted()
    val text   = z("""ZIO.succeed("Remedios Varo").tap(_ => x)""")
    val result = z("""ZIO.succeed("Remedios Varo").zipLeft(x)""")
    testQuickFixes(text, result, hint)
  }

  def test_block_tap_to_zipLeft(): Unit = {
    z {
      s"""ZIO.succeed("Remedios Varo").${START}tap { _ =>
         |  x
         |  x
         |  x
         |}$END""".stripMargin
    }.assertHighlighted()

    val text = z {
      """ZIO.succeed("Remedios Varo").tap { _ =>
        |  x
        |  x
        |  x
        |}""".stripMargin
    }
    val result = z {
      """ZIO.succeed("Remedios Varo").zipLeft {
        | x
        | x
        | x
        |}""".stripMargin
    }
    testQuickFixes(text, result, hint)
  }

  def test_tap_not_discarding_should_not_highlight(): Unit =
    z(s"""ZIO.succeed("Tarsila do Amaral").${START}tap(x => x)$END""").assertNotHighlighted()

}

class SimplifyFlatmapWithZipRightOperatorTest extends ZipInspectionTest("*>") {

  def test_infix_flatMap_to_*>(): Unit = {
    z(s"""ZIO.succeed("Xul Solar").${START}flatMap(_ => x)$END""").assertHighlighted()
    val text   = z("""ZIO.succeed("Xul Solar").flatMap(_ => ZIO succeed x)""")
    val result = z("""ZIO.succeed("Xul Solar") *> (ZIO succeed x)""")
    testQuickFixes(text, result, hint)
  }

  def test_block_infix_flatMap_to_*>(): Unit = {
    z {
      s"""ZIO.succeed("Xul Solar").${START}flatMap { _ =>
         |  ZIO succeed x
         |  ZIO succeed x
         |  ZIO succeed x
         |}$END""".stripMargin
    }.assertHighlighted()
    val text = z {
      """ZIO.succeed("Xul Solar").flatMap { _ =>
        |  ZIO succeed x
        |  ZIO succeed x
        |  ZIO succeed x
        |}""".stripMargin
    }
    val result = z {
      """ZIO.succeed("Xul Solar") *> {
        |  ZIO succeed x
        |  ZIO succeed x
        |  ZIO succeed x
        |}""".stripMargin
    }
    testQuickFixes(text, result, hint)
  }

  def test_flatMap_to_*>(): Unit = {
    z(s"""ZIO.succeed("Frida Kahlo").${START}flatMap(_ => x)$END""").assertHighlighted()
    val text   = z("""ZIO.succeed("Frida Kahlo").flatMap(_ => ZIO.succeed(x))""")
    val result = z("""ZIO.succeed("Frida Kahlo") *> ZIO.succeed(x)""")
    testQuickFixes(text, result, hint)
  }

  def test_block_flatMap_to_*>(): Unit = {
    z {
      s"""ZIO.succeed("Frida Kahlo").${START}flatMap { _ =>
         |  ZIO.succeed(x)
         |  ZIO.succeed(x)
         |  ZIO.succeed(x)
         |}$END""".stripMargin
    }.assertHighlighted()
    val text = z {
      """ZIO.succeed("Frida Kahlo").flatMap { _ =>
        |  ZIO.succeed(x)
        |  ZIO.succeed(x)
        |  ZIO.succeed(x)
        |}""".stripMargin
    }
    val result = z {
      """ZIO.succeed("Frida Kahlo") *> {
        |  ZIO.succeed(x)
        |  ZIO.succeed(x)
        |  ZIO.succeed(x)
        |}""".stripMargin
    }
    testQuickFixes(text, result, hint)
  }

  def test_flatMap_not_discarding_should_not_highlight(): Unit =
    z(s"""ZIO.succeed("Benito Quinquela Martín").${START}flatMap(x => x)$END""").assertNotHighlighted()

}

class SimplifyTapWithZipLeftOperatorTest extends ZipInspectionTest("<*") {

  def test_infix_tap_to_<*(): Unit = {
    z(s"""ZIO.succeed("Xul Solar").${START}tap(_ => x)$END""").assertHighlighted()
    val text   = z("""ZIO.succeed("Xul Solar").tap(_ => ZIO succeed x)""")
    val result = z("""ZIO.succeed("Xul Solar") <* (ZIO succeed x)""")
    testQuickFixes(text, result, hint)
  }

  def test_block_infix_tap_to_<*(): Unit = {
    z {
      s"""ZIO.succeed("Xul Solar").${START}tap { _ =>
         |  ZIO succeed x
         |  ZIO succeed x
         |  ZIO succeed x
         |}$END""".stripMargin
    }.assertHighlighted()
    val text = z {
      """ZIO.succeed("Xul Solar").tap { _ =>
        |  ZIO succeed x
        |  ZIO succeed x
        |  ZIO succeed x
        |}""".stripMargin
    }
    val result = z {
      """ZIO.succeed("Xul Solar") <* {
        |  ZIO succeed x
        |  ZIO succeed x
        |  ZIO succeed x
        |}""".stripMargin
    }
    testQuickFixes(text, result, hint)
  }

  def test_tap_to_<*(): Unit = {
    z(s"""ZIO.succeed("Frida Kahlo").${START}tap(_ => x)$END""").assertHighlighted()
    val text   = z("""ZIO.succeed("Frida Kahlo").tap(_ => ZIO.succeed(x))""")
    val result = z("""ZIO.succeed("Frida Kahlo") <* ZIO.succeed(x)""")
    testQuickFixes(text, result, hint)
  }

  def test_block_tap_to_<*(): Unit = {
    z {
      s"""ZIO.succeed("Frida Kahlo").${START}tap { _ =>
         |  ZIO.succeed(x)
         |  ZIO.succeed(x)
         |  ZIO.succeed(x)
         |}$END""".stripMargin
    }.assertHighlighted()
    val text = z {
      """ZIO.succeed("Frida Kahlo").tap { _ =>
        |  ZIO.succeed(x)
        |  ZIO.succeed(x)
        |  ZIO.succeed(x)
        |}""".stripMargin
    }
    val result = z {
      """ZIO.succeed("Frida Kahlo") <* {
        |  ZIO.succeed(x)
        |  ZIO.succeed(x)
        |  ZIO.succeed(x)
        |}""".stripMargin
    }
    testQuickFixes(text, result, hint)
  }

  def test_tap_not_discarding_should_not_highlight(): Unit =
    z(s"""ZIO.succeed("Benito Quinquela Martín").${START}tap(x => x)$END""").assertNotHighlighted()

}
