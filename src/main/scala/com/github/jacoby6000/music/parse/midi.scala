package com.github.jacoby6000.music.parse

import scodec.{bits => _, _}
import scodec.codecs._
import scodec.bits._
import shapeless.{:+:, CNil, Coproduct, Inl, Inr}

object midi {
  object ChunkType {
    val bytes = 4L
  }

  sealed trait ChunkType
  case object MThd extends ChunkType
  case object MTrk extends ChunkType
  case class UnknownChunkType(name: String) extends ChunkType

  sealed trait Format
  case object SingleTrack extends Format
  case object MultiTrack extends Format
  case object MultiTrackSequential extends Format
  
  sealed trait Division
  case class TicksPerQuarter(ticks: Int) extends Division
  case class Frames(framesPerSecond: Int, ticksPerFrame: Int) extends Division

  sealed trait Event
  sealed trait MidiEvent extends Event
  sealed trait MidiVoiceEvent extends MidiEvent
  sealed trait ThreeParamMidiEvent extends MidiVoiceEvent
  sealed trait TwoParamMidiEvent extends MidiVoiceEvent
  sealed trait MidiChannelModeEvent extends MidiEvent
  sealed trait SysexEvent extends Event
  sealed trait MetaEvent extends Event

  case class NoteOff(channel: Int, key: Int, velocity: Int) extends ThreeParamMidiEvent
  case class NoteOn(channel: Int, key: Int, velocity: Int) extends ThreeParamMidiEvent
  case class KeyPressure(channel: Int, key: Int, pressure: Int) extends ThreeParamMidiEvent
  case class ControllerChange(channel: Int, controller: Int, value: Int) extends ThreeParamMidiEvent
  case class ProgramChange(channel: Int, program: Int) extends TwoParamMidiEvent
  case class ChannelKeyPressure(channel: Int, pressure: Int) extends TwoParamMidiEvent
  case class PitchBend(channel: Int, leastSig: Int, mostSig: Int) extends ThreeParamMidiEvent

  case class AllSoundOff(channel: Int) extends MidiChannelModeEvent
  case class ResetAllControllers(channel: Int) extends MidiChannelModeEvent
  case class LocalControl(channel: Int, on: Boolean) extends MidiChannelModeEvent
  case class AllNotesOff(channel: Int) extends MidiChannelModeEvent
  case class OmniModeOff(channel: Int) extends MidiChannelModeEvent
  case class OmniModeOn(channel: Int) extends MidiChannelModeEvent
  case class MonoModeOn(channel: Int, channels: Int) extends MidiChannelModeEvent
  case class PolyModeOn(channel: Int) extends MidiChannelModeEvent

  case class F0Sysex(data: ByteVector) extends SysexEvent
  case class F7Sysex(data: ByteVector) extends SysexEvent

  case class SequenceNumber(sequenceNumber: Int) extends MetaEvent
  case class TextEvent(text: String) extends MetaEvent
  case class CopyrightNotice(text: String) extends MetaEvent
  case class SequenceName(text: String) extends MetaEvent
  case class InstrumentName(text: String) extends MetaEvent
  case class Lyric(text: String) extends MetaEvent
  case class Marker(text: String) extends MetaEvent
  case class CuePoint(text: String) extends MetaEvent
  case class MIDIChannelPrefix(channel: Int) extends MetaEvent
  case object EndOfTrack extends MetaEvent
  case class SetTempo(tempo: Int) extends MetaEvent
  case class SMTPEOffset(hours: Int, minutes: Int, seconds: Int, frames: Int, hundredthFrames: Int) extends MetaEvent
  case class TimeSignature(numerator: Int, denominator: Int, clocksPerMetronome: Int, thirtySecondNotesPerTwentyFourMidiClocks: Int) extends MetaEvent
  case class KeySignature(numAccidentals: Int, minor: Boolean) extends MetaEvent
  case class SequencerSpecificMetaEvent(id: Int, data: ByteVector) extends MetaEvent

  case class VariableLengthQuantity(n: Int)

  case class Header(chunkType: ChunkType, format: Format, tracks: Int, division: Division)
  case class Track(chunkType: ChunkType, deltaTime: VariableLengthQuantity, event: Event)

  val chunkTypeCodec: Codec[ChunkType] = {
    fixedSizeBytes(ChunkType.bytes, ascii).xmap[ChunkType](
      {
        case "MThd" => MThd
        case "MTrk" => MTrk
        case s => UnknownChunkType(s)
      },
      {
        case MThd => "MThd"
        case MTrk => "MTrk"
        case UnknownChunkType(s) => s.take(4)
      }
    )
  }

  val variableLengthQuantity: Codec[VariableLengthQuantity] =
    new Codec[VariableLengthQuantity] {
      val sizeBound: SizeBound = SizeBound(8, Some(32))

      def encode(value: VariableLengthQuantity): Attempt[BitVector] = {
        val resultThing =
          value.n.toBinaryString
            .sliding(7, 7)
            .map(_.reverse.padTo(7, "0").reverse.mkString)
            .map(x => if (x.contains("1")) x + "1" else x + "0")
            .toVector

        val refinedResult =
          resultThing
            .find(_.endsWith("0"))
            .map(_ => resultThing)
            .getOrElse {
              val last = resultThing.last
              val updatedLast = last.dropRight(1) + "0"
              resultThing.dropRight(1) :+ updatedLast
            }.map(Integer.parseInt(_, 2).toByte)

        Attempt.successful(BitVector(refinedResult.toArray))
      }

      def decode(bits: BitVector): Attempt[DecodeResult[VariableLengthQuantity]] = {
        def go(bs: BitVector, carry: Int, consumed: Int): Either[String, (Int, BitVector)] = {
          bs.consume(8) { bv =>
            val byte = bv.toByte(false)
            val numeric = byte >> 1
            val continue = if (byte - (numeric << 1) == 1) true else false

            Right((carry << 7 + numeric, continue))
          }.flatMap {
            case (bv, (c, true)) => go(bv, c, consumed + 8)
            case (bv, (c, false)) => Right((c, bv))
          }
        }

        Attempt.fromEither(go(bits, 0, 0).fold(e => Left(Err(e)), x => Right(DecodeResult(VariableLengthQuantity(x._1), x._2))))
      }
    }

  private val variableInt: Codec[Int] = variableLengthQuantity.xmap(_.n,VariableLengthQuantity)

  val formatCodec: Codec[Format] = {
    uint16.exmap(
      {
        case 0 => Attempt.successful(SingleTrack)
        case 1 => Attempt.successful(MultiTrack)
        case 2 => Attempt.successful(MultiTrackSequential)
        case x =>
          Attempt.failure(Err(s"Invalid midi format found in headers. Expected 0, 1 or 2. Got $x"))
      },
      {
        case SingleTrack => Attempt.successful(0)
        case MultiTrack => Attempt.successful(1)
        case MultiTrackSequential => Attempt.successful(2)
      }
    )
  }

  val divisionCodec: Codec[Division] = {
    type CoPro = ((Int, Int), Unit) :+: (Int, Unit) :+: CNil
    ((uint8 ~ int(7) ~ constant(bin"1")) :+: (uint(15) ~ constant(bin"0"))).xmap[Division](
      {
        case Inl(((f1, f2), _)) => Frames(f1, Math.abs(f2))
        case Inr(Inl((f1, _))) => println("ticking") ; TicksPerQuarter(f1)
        case Inr(Inr(_)) => throw new Exception("This is literally actually impossible.")
      },
      {
        case Frames(f1, f2) => Coproduct[CoPro](((f1, -f2), ()))
        case TicksPerQuarter(n) => Coproduct[CoPro](((n >> 8, n), ()))
      }
    ).choice
  }

  val midiEventCodec: Codec[MidiEvent] = {

    val handleMidiChannelModeEventDecode: ((Int, Int), Int) => Attempt[MidiChannelModeEvent] = {
      case ((n, 0x78), 0x00) => Attempt.successful(AllSoundOff(n))
      case ((n, 0x79), 0x00) => Attempt.successful(ResetAllControllers(n))
      case ((n, 0x7A), 0x00) => Attempt.successful(LocalControl(n, false))
      case ((n, 0x7A), 0x7F) => Attempt.successful(LocalControl(n, true))
      case ((n, 0x7A), x) =>
        Attempt.failure(Err(
          s"Invalid value supplied for byte 3 in Midi Channel Mode Event 'Local Control' (0x7A)" +
            s"for channel ${n.toHexString.toUpperCase}. " +
            s"Got ${x.toHexString.toUpperCase} expected 0x00 (off) or 0x7F (on)."
        ))
      case ((n, 0x7B), 0x00) => Attempt.successful(AllNotesOff(n))
      case ((n, 0x7C), 0x00) => Attempt.successful(OmniModeOff(n))
      case ((n, 0x7D), 0x00) => Attempt.successful(OmniModeOn(n))
      case ((n, 0x7E), m) => Attempt.successful(MonoModeOn(n, m))
      case ((n, 0x7F), 0x00) => Attempt.successful(PolyModeOn(n))
      case ((_, adr), m) =>
        Attempt.failure(Err(
          s"Invalid command issued for MidiChannelModeEvent. " +
            s"Got command id ${adr.toHexString.toUpperCase} " +
            s"with data ${m.toHexString.toUpperCase}. " +
            s"The Command id should be between 0x78 and 0x7F. If your command was in this range, " +
            s"ensure that the correct data value (usually 0x00) is set."
        ))
    }

    val handleMidiChannelModeEventEncode: MidiChannelModeEvent => Attempt[((Int, Int), Int)] = {
      case AllSoundOff(n) => Attempt.successful(((n, 0x78), 0x00))
      case ResetAllControllers(n) => Attempt.successful(((n, 0x79), 0x00))
      case LocalControl(n, false) => Attempt.successful(((n, 0x7A), 0x00))
      case LocalControl(n, true) => Attempt.successful(((n, 0x7A), 0x7F))
      case AllNotesOff(n) => Attempt.successful(((n, 0x7B), 0x00))
      case OmniModeOff(n) => Attempt.successful(((n, 0x7C), 0x00))
      case OmniModeOn(n) => Attempt.successful(((n, 0x7D), 0x00))
      case MonoModeOn(n, m) => Attempt.successful(((n, 0x7E), m))
      case PolyModeOn(n) => Attempt.successful(((n, 0x7F), 0x00))
    }

    val handleThreeParamEventDecode: (((Int, Int), Int), Int) => Attempt[ThreeParamMidiEvent] = {
        case (((0x8, n), k), v) => Attempt.successful(NoteOff(n, k, v))
        case (((0x9, n), k), v) => Attempt.successful(NoteOn(n, k, v))
        case (((0xA, n), k), v) => Attempt.successful(KeyPressure(n, k, v))
        case (((0xB, n), k), v) => Attempt.successful(ControllerChange(n, k, v))
        case (((0xE, n), k), v) => Attempt.successful(PitchBend(n, k, v))
        case (((adr, n), k), v) =>
          Attempt.failure(Err(
            s"Invalid command specified. statusByte: " +
              s"${(adr << 4 + n).toHexString.toUpperCase}, " +
              s"n: ${n.toHexString.toUpperCase}, " +
              s"k: ${k.toHexString.toUpperCase}, " +
              s"v: ${v.toHexString.toUpperCase}"
          ))
      }

    val handleThreeParamEventEncode: ThreeParamMidiEvent => Attempt[(((Int, Int), Int), Int)] = {
      case NoteOff(n, k, v) => Attempt.successful((((0x8, n), k), v))
      case NoteOn(n, k, v) => Attempt.successful((((0x9, n), k), v))
      case KeyPressure(n, k, v) => Attempt.successful((((0xA, n), k), v))
      case ControllerChange(n, k, v) => Attempt.successful((((0xB, n), k), v))
      case PitchBend(n, k, v) => Attempt.successful((((0xE, n), k), v))
    }

    val handleTwoParamEventDecode: ((Int, Int), Int) => Attempt[TwoParamMidiEvent] = {
      case ((0xC, n), p) => Attempt.successful(ProgramChange(n, p))
      case ((0xD, n), p) => Attempt.successful(ChannelKeyPressure(n, p))
      case ((adr, n), p) =>
        Attempt.failure(Err(
          s"Invalid command specified. statusByte: " +
            s"${Array(ByteVector(adr.toByte << 4 + n.toByte))}, " +
            s"n: $n, " +
            s"k: $p"
        ))
    }

    val handleTwoParamEventEncode: TwoParamMidiEvent => Attempt[((Int, Int), Int)] = {
      case ProgramChange(n, p) => Attempt.successful(((0xC, n), p))
      case ChannelKeyPressure(n, p) => Attempt.successful(((0xD, n), p))
    }

    type CoPro = ((Int, Int), Int) :+: (((Int, Int), Int), Int) :+: CNil
    ((uint4 ~ uint4 ~ uint16) :+: (uint4 ~ uint4 ~ uint16 ~ uint16)).exmap[MidiEvent](
      {
        case Inl(x) => handleMidiChannelModeEventDecode(x) orElse handleTwoParamEventDecode(x)
        case Inr(Inl(x)) => handleThreeParamEventDecode(x)
        case Inr(Inr(_)) => throw new Exception("This is literally actually impossible.")
      },
      {
        case two: TwoParamMidiEvent => handleTwoParamEventEncode(two).map(Coproduct[CoPro](_))
        case chan: MidiChannelModeEvent => handleMidiChannelModeEventEncode(chan).map(Coproduct[CoPro](_))
        case three: ThreeParamMidiEvent => handleThreeParamEventEncode(three).map(Coproduct[CoPro](_))
      }
    ).choice
  }

  val sysexEventCodec: Codec[SysexEvent] = {
    (uint8 ~ variableSizeBytes(variableInt, bytes)).exmap(
      {
        case (0xF0, bytes) => Attempt.successful(F0Sysex(bytes))
        case (0xF7, bytes) => Attempt.successful(F7Sysex(bytes))
        case (addr, _) =>
          Attempt.failure(Err(
            s"Invalid Sysex event. Got ${addr.toHexString.toUpperCase}, expected 0xF0 or 0xF7."
          ))
      },
      {
        case F0Sysex(byteVec) => Attempt.successful((0xF0, byteVec))
        case F7Sysex(byteVec) => Attempt.successful((0xF7, byteVec))
      }
    )
  }

  val metaEventCodec: Codec[MetaEvent] = {
    val smtpeOffsetCodec =
      (uint16 ~ uint16 ~ uint16 ~ uint16 ~ uint16).xmap[SMTPEOffset](
        {
          case ((((h, m), s), fr), ff) => SMTPEOffset(h, m, s, fr, ff)
        },
        {
          case SMTPEOffset(h, m, s, fr, ff) => ((((h, m), s), fr), ff)
        }
      )

    (constant(hex"0xFF") ~ uint8 ~ variableSizeBytes(variableInt, bits)).exmap(
      {
        case (((), 0x00), bitVec) => uint16.decodeValue(bitVec).map(SequenceNumber(_))
        case (((), 0x01), bitVec) => ascii.decodeValue(bitVec).map(TextEvent(_))
        case (((), 0x02), bitVec) => ascii.decodeValue(bitVec).map(CopyrightNotice(_))
        case (((), 0x03), bitVec) => ascii.decodeValue(bitVec).map(SequenceName(_))
        case (((), 0x04), bitVec) => ascii.decodeValue(bitVec).map(InstrumentName(_))
        case (((), 0x05), bitVec) => ascii.decodeValue(bitVec).map(Lyric(_))
        case (((), 0x06), bitVec) => ascii.decodeValue(bitVec).map(Marker(_))
        case (((), 0x07), bitVec) => ascii.decodeValue(bitVec).map(CuePoint(_))
        case (((), 0x20), bitVec) =>
          (uint8 ~ uint8).decodeValue(bitVec).flatMap {
            case (0x01, cc) => Attempt.successful(MIDIChannelPrefix(cc))
            case (addr, _) => Attempt.failure(Err(
              s"Failed to decode meta event. " +
                s"Got ${addr.toHexString.toUpperCase} expected 0x01 for event 0x20."
            ))
          }
        case (((), 0x2F), _) => Attempt.successful(EndOfTrack)
        case (((), 0x51), bitVec) => uint24.decodeValue(bitVec).map(SetTempo(_))
        case (((), 0x54), bitVec) => smtpeOffsetCodec.decodeValue(bitVec)
        case (((), 0x58), bitVec) =>
          (uint16 ~ uint16 ~ uint16 ~ uint16).decodeValue(bitVec).map {
            case (((nn, dd), cc), bb) => TimeSignature(nn, dd, cc, bb)
          }
        case (((), 0x59), bitVec) =>
          (int16 ~ uint16).decodeValue(bitVec).flatMap {
            case (sf, 0) => Attempt.successful(KeySignature(sf, false))
            case (sf, 1) => Attempt.successful(KeySignature(sf, true))
            case (_, x) => Attempt.failure(Err(
              s"Invalid input for Meta Event 0x59. Got $x for major/minor mode, expected 0 or 1."
            ))
          }
        case (((), 0x7F), bitVec) =>
          (constrainedVariableSizeBytes(int(3), int24, 1, 3) ~ bytes).decodeValue(bitVec).map {
            case (n, leftover) => SequencerSpecificMetaEvent(n, leftover)
          }
      },
      {
        case SequenceNumber(n) => uint16.encode(n).map(x => (((), 0x00), x))
        case TextEvent(text) => ascii.encode(text).map((((), 0x01), _))
        case CopyrightNotice(n) => ascii.encode(n).map((((), 0x02), _))
        case SequenceName(name) => ascii.encode(name).map((((), 0x03), _))
        case InstrumentName(name) => ascii.encode(name).map((((), 0x04), _))
        case Lyric(text) => ascii.encode(text).map((((), 0x05), _))
        case Marker(text) => ascii.encode(text).map((((), 0x06), _))
        case CuePoint(text) => ascii.encode(text).map((((), 0x07), _))
        case MIDIChannelPrefix(cc) => (uint8 ~ uint8).encode((0x01, cc)).map((((), 0x20), _))
        case EndOfTrack => Attempt.successful((((), 0x2F), BitVector(0x00)))
        case SetTempo(t) => uint24.encode(t).map((((), 0x51), _))
        case o: SMTPEOffset => smtpeOffsetCodec.encode(o).map((((), 0x54), _))
        case TimeSignature(nn, dd, cc, bb) => (uint16 ~ uint16 ~ uint16 ~ uint16).encode((((nn, dd), cc), bb)).map((((), 0x58), _))
        case KeySignature(sf, bool) => (int16 ~ uint16).encode((sf, if(bool) 1 else 0)).map((((), 0x59), _))
        case SequencerSpecificMetaEvent(id, vec) => (constrainedVariableSizeBytes(int(3), int24, 1, 3) ~ bytes).encode((id, vec)).map((((), 0x7F), _))
      }
    )
  }

  val headerCodec: Codec[Header] =
    (chunkTypeCodec ::
      variableSizeBytesLong(
        uint32,
        formatCodec.withContext("Format Codec") ::
          uint16.withContext("track count") ::
          divisionCodec.withContext("division")
      ).withContext("Header Length")
    ).as[Header]

  val eventCodec =
    new Codec[Event] {
      val sysex = sysexEventCodec.upcast[Event]
      val meta = metaEventCodec.upcast[Event]
      val midi = midiEventCodec.upcast[Event]
      def sizeBound: SizeBound = SizeBound(12, None)
      def encode(value: Event): Attempt[BitVector] =
        sysex.encode(value) orElse
          meta.encode(value) orElse
          midi.encode(value)

      def decode(bits: BitVector): Attempt[DecodeResult[Event]] =
        sysex.decode(bits) orElse
          meta.decode(bits) orElse
          midi.decode(bits)
    }

  val trackCodec: Codec[Track] =
    (chunkTypeCodec :: variableLengthQuantity :: eventCodec).as[Track]

  val fileCodec =
    headerCodec ~ list(trackCodec)
}
