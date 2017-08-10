package com.github.jacoby6000.music

import java.nio.file.Files
import java.nio.file.Paths
import com.github.jacoby6000.maestro.midi

import scodec.bits.BitVector

object MidiDecode extends App {
  val bits = BitVector(Files.readAllBytes(Paths.get("/Users/jacobbarber/Downloads/lttlef.mid")))
  println(midi.decodeMidi(bits).map(midi.normalizeOnOff))
}
