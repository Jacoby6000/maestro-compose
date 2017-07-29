package com.github.jacoby6000.music

import java.nio.file.Files
import java.nio.file.Paths

import scodec.bits.BitVector

object MidiDecode extends App {
  val bytes = Files.readAllBytes(Paths.get("/Users/jacobbarber/Downloads/MIDI_sample.mid"))
  println(parse.midi.fileCodec.decodeValue(BitVector(bytes)))
}
