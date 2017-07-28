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
        case A(Normal) => E(Normal)
        case A(Sharp) => F(Normal)
        case B(Flat) => F(Normal)
        case B(Normal) => F(Sharp)
        case C(Normal) => G(Normal)
        case C(Sharp) => G(Sharp)
        case D(Flat) => A(Flat)
        case D(Normal) => A(Normal)
        case D(Sharp) => A(Sharp)
        case E(Flat) => B(Flat)
        case E(Normal) => B(Normal)
        case F(Normal) => C(Normal)
        case F(Sharp) => C(Sharp)
        case G(Flat) => D(Flat)
        case G(Normal) => D(Normal)
        case G(Sharp) => D(Sharp)
      }
  }
}
