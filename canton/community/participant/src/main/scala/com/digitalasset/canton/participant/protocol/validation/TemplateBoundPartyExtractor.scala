// Copyright (c) 2026 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.digitalasset.canton.participant.protocol.validation

import com.digitalasset.canton.LfPartyId
import com.digitalasset.canton.data.{ActionDescription, FullTransactionViewTree}
import com.digitalasset.canton.discard.Implicits.*
import com.digitalasset.canton.protocol.CreatedContract

/** Extracts per-party template IDs from a parsed transaction request.
  *
  * For each party that is a signatory or actor in the transaction, collects
  * the template IDs of all actions where that party participates. This is
  * the input to TemplateBoundPartyValidator.
  */
object TemplateBoundPartyExtractor {

  /** Extract a mapping from party to the set of template IDs of actions
    * where that party is a signatory or actor.
    *
    * @param rootViewTrees the root view trees from the parsed transaction request
    * @return map from party ID to the template IDs of their actions
    */
  def extractTemplateIdsByParty(
      rootViewTrees: Seq[FullTransactionViewTree]
  ): Map[LfPartyId, Set[String]] = {
    val builder = scala.collection.mutable.Map.empty[LfPartyId, Set[String]]

    rootViewTrees.foreach { viewTree =>
      val vpd = viewTree.viewParticipantData
      processAction(vpd.actionDescription, vpd.createdCore, builder)
    }

    builder.toMap
  }

  /** Process a single action description and accumulate party-to-template mappings.
    * Extracted as a separate method for testability without FullTransactionViewTree.
    */
  private[validation] def processAction(
      actionDescription: ActionDescription,
      createdCore: Seq[CreatedContract],
      builder: scala.collection.mutable.Map[LfPartyId, Set[String]],
  ): Unit =
    actionDescription match {
      case exercise: ActionDescription.ExerciseActionDescription =>
        val templateIdStr = exercise.templateId.toString
        exercise.actors.foreach { party =>
          builder.updateWith(party) {
            case Some(existing) => Some(existing + templateIdStr)
            case None => Some(Set(templateIdStr))
          }.discard
        }

      case _: ActionDescription.CreateActionDescription =>
        createdCore.foreach { created =>
          val templateIdStr = created.contract.templateId.toString
          created.contract.signatories.foreach { party =>
            builder.updateWith(party) {
              case Some(existing) => Some(existing + templateIdStr)
              case None => Some(Set(templateIdStr))
            }.discard
          }
        }

      case _: ActionDescription.FetchActionDescription =>
        ()

      case _: ActionDescription.LookupByKeyActionDescription =>
        ()
    }
}
