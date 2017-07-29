package com.github.jacoby6000.music

import core.ast._
import key.annotations._

object CircleOfFifths extends App {
  def accidentalToString(accidental: Accidental): String =
    accidental match {
      case Sharp => "♯"
      case Flat => "♭"
      case Natural => "♮"
    }

  def pitchToString(pitch: Pitch): String =
    pitch match {
      case A(accidental) => "A" + accidentalToString(accidental)
      case B(accidental) => "B" + accidentalToString(accidental)
      case C(accidental) => "C" + accidentalToString(accidental)
      case D(accidental) => "D" + accidentalToString(accidental)
      case E(accidental) => "E" + accidentalToString(accidental)
      case F(accidental) => "F" + accidentalToString(accidental)
      case G(accidental) => "G" + accidentalToString(accidental)
    }

  println(Stream.iterate(C(Natural): Pitch)(Root(_).perfect).map(pitchToString).take(100).toVector.mkString("\n"))
}
