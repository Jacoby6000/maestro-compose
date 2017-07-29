package com.github.jacoby6000.music.core

object ast {
  case class KeySignature(note: Duration, beats: Int)

  sealed trait Accidental
  sealed trait Flattie extends Accidental // Until I come up with a better name...
  sealed trait Sharpie extends Accidental // Same here
  case object Flat extends Flattie
  case object Sharp extends Sharpie
  case object Natural extends Flattie with Sharpie with Accidental // All notes can be natural

  sealed trait Pitch
  case class A(accidental: Accidental) extends Pitch
  case class B(accidental: Flattie) extends Pitch
  case class C(accidental: Sharpie) extends Pitch
  case class D(accidental: Accidental) extends Pitch
  case class E(accidental: Flattie) extends Pitch
  case class F(accidental: Sharpie) extends Pitch
  case class G(accidental: Accidental) extends Pitch

  sealed trait Duration
  sealed trait StandaloneDuration extends Duration
  case object Sixteenth extends StandaloneDuration
  case object Eighth extends StandaloneDuration
  case object Quarter extends StandaloneDuration
  case object Half extends StandaloneDuration
  case object Whole extends StandaloneDuration
  case class Dotted(duration: StandaloneDuration, dots: Int) extends Duration

  sealed trait Note
  case class LeafNote(pitch: Pitch, duration: Duration, octave: Int) extends Note
  case class Tie(left: Note, right: Note) extends Note

  sealed trait Dynamics
  sealed trait Volume extends Dynamics
  case object Fortississimo extends Volume
  case object Fortissimo extends Volume
  case object Forte extends Volume
  case object Piano extends Volume
  case object Pianissimo extends Volume
  case object Pianississimo extends Volume
  case class Creshendo(from: Volume, to: Volume) extends Dynamics

  sealed trait Segment[T]
  case class Repeated[T](segment: T, times: Int) extends Segment[T]
  case class Section[T](time: KeySignature, dynamics: Dynamics, segment: T) extends Segment[T]
  case class Notes[T](notes: List[Note]) extends Segment[T]
  case class Together[T](segments: List[T]) extends Segment[T]
  case class And[T](left: T, right: T) extends Segment[T]
}
