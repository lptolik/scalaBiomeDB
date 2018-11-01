package BioGraph.sbml

import org.apache.logging.log4j.LogManager
import org.sbml.jsbml.SBMLDocument

object JSBMLExportCommunity {
  private val logger = LogManager.getLogger(this.getClass.getName)

  def assembleCommunityModel(models: Seq[SBMLDocument]): SBMLDocument = {
    //TODO 1. Determine all extracellular compounds
    //TODO 2. Group (1) by XRef links
    //TODO 3. Look how many compounds are in more than one group
    //TODO 4. Assemble community model in the same way as single cell,
    //TODO    excepting extracellular metabolites: take only one from each group
    //TODO 5. Need to refactor JSBMLExport somehow
    ???
  }
}
