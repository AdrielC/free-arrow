package com.adrielc.quivr.syntax

import com.adrielc.quivr.{ArrowChoicePlus, ArrowChoiceZero, ArrowPlus, ArrowZero}

object all extends AllSyntax

trait AllSyntax
  extends ArrowChoicePlus.ToArrowChoicePlusOps
  with ArrowChoiceZero.ToArrowChoiceZeroOps
  with ArrowPlus.ToArrowPlusOps
  with ArrowZero.ToArrowZeroOps
