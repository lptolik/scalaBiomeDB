package BioGraph

import psidev.psi.mi.xml.io.impl.PsimiXmlReader253
import psidev.psi.mi.xml.model.{ExperimentDescription, ExperimentalInteractor, Interaction, Interactor, EntrySet}
import psidev.psi.mi.xml.{PsimiXmlWriter, PsimiXmlReader, PsimiXmlLightweightReader}
import psidev.psi.mi.xml.xmlindex.impl.PsimiXmlPullParser253

import java.io.File
import scala.collection.JavaConverters._

/**
  * Created by artem on 16.06.16.
  */
class IntactUtil(dir: String) {
  val fl = new File(dir)
  val reader = new PsimiXmlReader()
  val readResult = reader.read(fl).getEntries.asScala

  def getInteractors = {
    readResult.map(_.getInteractors.asScala)
  }

  def getInteractions = {
    readResult.map(_.getInteractions.asScala)
  }

  def getExperiments = {
    readResult.map(_.getExperiments.asScala)
  }

  def interactorInfo(interactor: Interactor) = {
    val id = interactor.getId
    val name = interactor.getNames.getFullName
    val geneName = interactor.getNames.getAliases
    val seq = interactor.getSequence
    val xrefs = interactor.getXref
    val seqNode = new Sequence(seq)

    List(id, seqNode)
  }

  def interactionInfo(interaction: Interaction) = {
    val participants = interaction.getParticipants.asScala.map(_.getInteractor.getId)
    val experiments = interaction.getExperiments.asScala.map(_.getId).head
    List(participants, experiments)
  }

  def experimentInfo(experiment: ExperimentDescription) = {
    val expId = experiment.getId
    val expNames = experiment.getNames.getFullName
    val participantDetectionMethod = experiment.getParticipantIdentificationMethod.getNames.getFullName
    val interactionDetectionMethod = experiment.getInteractionDetectionMethod.getNames.getFullName
//    val expId = experiment.getExperimentRefs
//    val expNames = experiment.getExperiments.asScala.map(_.getNames).head
    List(expId, participantDetectionMethod, interactionDetectionMethod, expNames)
  }

}
