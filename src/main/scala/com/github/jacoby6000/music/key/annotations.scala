package com.github.jacoby6000.music.key

import com.github.jacoby6000.music.core.ast._

object annotations {

  sealed trait Triad
  case class MajorTriad(root: Pitch, additions: List[RelativePitch])
  case class MinorTriad(root: Pitch, additions: List[RelativePitch])
  case class DiminishedTriad(root: Pitch, additions: List[RelativePitch])
  case class AugmentedTriad(root: Pitch, additions: List[RelativePitch])


  sealed trait RelativePitch
  case object Second extends RelativePitch
  case object MajorThird extends RelativePitch
  case object MinorThird extends RelativePitch
  case object Fourth extends RelativePitch
  case object Fifth extends RelativePitch
  case object Sixth extends RelativePitch
  case object Seventh extends RelativePitch
  case object Octave extends RelativePitch


  case class Root(pitch: Pitch) {
    def perfect: Pitch =
      pitch match {
        case A(Flat) => E(Flat)
        case A(Natural) => E(Natural)
        case A(Sharp) => F(Natural)
        case B(Flat) => F(Natural)
        case B(Natural) => F(Sharp)
        case C(Natural) => G(Natural)
        case C(Sharp) => G(Sharp)
        case D(Flat) => A(Flat)
        case D(Natural) => A(Natural)
        case D(Sharp) => A(Sharp)
        case E(Flat) => B(Flat)
        case E(Natural) => B(Natural)
        case F(Natural) => C(Natural)
        case F(Sharp) => C(Sharp)
        case G(Flat) => D(Flat)
        case G(Natural) => D(Natural)
        case G(Sharp) => D(Sharp)
      }
  }
}
