package BioGraph.sbml

import java.io.File

import org.sbml.jsbml.SBMLReader
import org.sbml.jsbml.ext.fbc.FBCModelPlugin
import utilFunctions.TransactionSupport

import scala.io.Source
import scala.collection.JavaConverters._
import scala.util.Try

/**
  * Created by piane_ramso on 12/23/16.
  */
object Test extends App with TransactionSupport {
//  val bp = "/Users/ramso/Yandex.Disk.localized/Studying/PhD/thesis/pushchino_phd/" +
//    "agora/AGORA-1.01-Reconstructions/patric-fams-2016-0904-reduced2/"

  val bp = args(0)

  //  val modelsDir = "/Users/ramso/Yandex.Disk.localized/Studying/PhD/thesis/pushchino_phd/agora/AGORA-1.01-Reconstructions/"
  val modelsDir = args(1)

  println("started parsing fasta")

  val existingIds = Seq("nr.0001", "nr.0002", "nr.0003", "nr.0004").flatMap { file =>
    Source
      .fromFile(bp + file)
      .getLines()
      //    .take(1000)
      .filter(_.startsWith(">"))
      .map(_.replace(">fig|", "").trim)
  }.toSet

//  existingIds
//    .groupBy(_.split('.').drop(2).mkString("."))
//    .toList
//    .sortBy(-_._2.size)
//    .take(100)
//    .foreach { case (key, values) =>
//      println(s"peg: ${key}, size ${values.size}")
//    }
//  throw new Exception()

  println(s"fasta parsed, existingIds in file: ${existingIds.size}")

  val preRes = new File(modelsDir)
    .listFiles()
    .filter(_.getName.endsWith(".xml"))
//    .take(1)
    .toList
    .par
    .map { modelFile =>
      Try {
        val geneProducts = new SBMLReader()
          .readSBML(modelFile)
          .getModel
          .getPlugin("fbc")
          .asInstanceOf[FBCModelPlugin]
          .getListOfGeneProducts
          .asScala
          .toList

        val realTaxonId = geneProducts
          .filter(gp => gp.getLabel.contains("peg") && !gp.getLabel.startsWith("g."))
          .find(!_.getLabel.startsWith("0000000.0"))
          .map(_.getLabel.split('.').take(2).mkString("."))
          .get

        val shit = realTaxonId + "."

        val res = geneProducts.count { gp =>
          if (gp.getLabel.startsWith("g."))
            false
          else {
            val realLabel = shit + gp.getLabel.split('.').drop(2).mkString(".")
            existingIds.contains(realLabel)
          }
        }

        val ratio = 100 * res.toDouble / geneProducts.size
        println(s"file: ${modelFile.getName}, realTaxonId: $realTaxonId, " +
          s"geneProducts count: ${geneProducts.size}, found in fasta $res, found ratio ${ratio}%")

        (res, ratio, modelFile)
      }.toOption
      .getOrElse((0, 0d, modelFile))
    }

  preRes
    .toList
    .sortBy(-_._2)
    .foreach { case (_, r, f) =>
        println(f"$r%.2f ${f.getName}")
    }

  val figIdsExist = preRes.map(_._1).sum
  println(s"\n\n!!! >>>>>> found: $figIdsExist")
}

object Test1 extends App {
  val ourTexIds = Source
    .fromFile("/Users/ramso/Yandex.Disk.localized/Studying/PhD/thesis/pushchino_phd/1500_organisms/1500_organisms_tax_ids.txt")
    .getLines()
    .filter(_.nonEmpty)
    .map(_.toInt)
    .toSeq
    .sorted

  val otherIds = Source
    .fromFile("/Users/ramso/Yandex.Disk.localized/Studying/PhD/thesis/pushchino_phd/embl_gems/model_list.tsv")
    .getLines()
    .drop(1)
    .filter(_.nonEmpty)
    .map(a => (a.split("\t")(1).toInt, a.split("\t")(4)))
    .toMap

  ourTexIds
    .toSet
    .intersect(otherIds.keySet)
    .toSeq
    .map(tid => (tid, otherIds(tid)))
    .sortBy(_._2)
    .foreach { case (tid, n) => println(s"$tid $n") }
}
