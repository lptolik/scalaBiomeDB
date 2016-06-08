package BioGraph

import java.io.File

import org.biojava.nbio.core.exceptions.ParserException
import org.scalatest.FunSuite

/**
  * Created by artem on 30.05.16.
  */
class GenBankUtilTest extends FunSuite {

  test("catch genbank file with bad xref") {
    intercept[ParserException]{
      val gbReader = new GenBankUtil(new File("/home/artem/work/2016/scala_projects/BioGraph/src/test/scala/BioGraph/genbankFileWithBadXref.gb"))
      gbReader.getAccessionsFromGenBankFile
    }
  }

}
