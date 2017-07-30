package com.github.jacoby6000.music

import java.nio.file.Files
import java.nio.file.Paths

import scodec.bits.BitVector

object MidiDecode extends App {
  val bits = BitVector(Files.readAllBytes(Paths.get("/home/jbarber/Downloads/lttlef.mid")))
  println(bits.toHex)
  println(parse.midi.fileCodec.decodeValue(bits))
}
