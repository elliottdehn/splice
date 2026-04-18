// Copyright (c) 2026 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.digitalasset.canton.participant.protocol.validation

import com.digitalasset.canton.data.ActionDescription
import com.digitalasset.canton.protocol.DummySerializationVersion
import com.digitalasset.daml.lf.crypto.Hash
import com.digitalasset.daml.lf.data.Ref
import com.digitalasset.daml.lf.transaction.Versioned
import com.digitalasset.daml.lf.value.Value
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class TemplateBoundPartyExtractorTest extends AnyWordSpec with Matchers {

  private val party1 = Ref.Party.assertFromString("party1")
  private val party2 = Ref.Party.assertFromString("party2")
  private val contractId = Value.ContractId.V1(Hash.hashPrivateKey("test-contract"))
  private val seed = Hash.hashPrivateKey("test-seed")
  private val templateId = Ref.Identifier.assertFromString("pkg:Mod:Template")
  private val templateId2 = Ref.Identifier.assertFromString("pkg:Mod:Template2")
  private val version = DummySerializationVersion

  "TemplateBoundPartyExtractor" should {

    "return empty map for empty view trees" in {
      val result = TemplateBoundPartyExtractor.extractTemplateIdsByParty(Seq.empty)
      result shouldBe empty
    }

    "extract actors from exercise actions" in {
      val builder = scala.collection.mutable.Map.empty[com.digitalasset.canton.LfPartyId, Set[String]]
      val exercise = ActionDescription.ExerciseActionDescription.tryCreate(
        inputContractId = contractId,
        templateId = templateId,
        choice = Ref.ChoiceName.assertFromString("Swap"),
        interfaceId = None,
        packagePreference = Set.empty,
        chosenValue = Versioned(version, Value.ValueUnit),
        actors = Set(party1, party2),
        byKey = false,
        seed = seed,
        failed = false,
      )

      TemplateBoundPartyExtractor.processAction(exercise, Seq.empty, builder)

      builder.toMap shouldBe Map(
        party1 -> Set(templateId.toString),
        party2 -> Set(templateId.toString),
      )
    }

    "fetch action produces no mappings (read-only)" in {
      val builder = scala.collection.mutable.Map.empty[com.digitalasset.canton.LfPartyId, Set[String]]
      val fetch = ActionDescription.FetchActionDescription(
        inputContractId = contractId,
        actors = Set(party1),
        byKey = false,
        templateId = templateId,
        interfaceId = None,
      )

      TemplateBoundPartyExtractor.processAction(fetch, Seq.empty, builder)

      builder.toMap shouldBe empty
    }

    "accumulate multiple exercises for the same party" in {
      val builder = scala.collection.mutable.Map.empty[com.digitalasset.canton.LfPartyId, Set[String]]

      val exercise1 = ActionDescription.ExerciseActionDescription.tryCreate(
        inputContractId = contractId,
        templateId = templateId,
        choice = Ref.ChoiceName.assertFromString("Swap"),
        interfaceId = None,
        packagePreference = Set.empty,
        chosenValue = Versioned(version, Value.ValueUnit),
        actors = Set(party1),
        byKey = false,
        seed = seed,
        failed = false,
      )

      val exercise2 = ActionDescription.ExerciseActionDescription.tryCreate(
        inputContractId = contractId,
        templateId = templateId2,
        choice = Ref.ChoiceName.assertFromString("Transfer"),
        interfaceId = None,
        packagePreference = Set.empty,
        chosenValue = Versioned(version, Value.ValueUnit),
        actors = Set(party1),
        byKey = false,
        seed = Hash.hashPrivateKey("seed2"),
        failed = false,
      )

      TemplateBoundPartyExtractor.processAction(exercise1, Seq.empty, builder)
      TemplateBoundPartyExtractor.processAction(exercise2, Seq.empty, builder)

      builder(party1) shouldBe Set(templateId.toString, templateId2.toString)
    }
  }
}
