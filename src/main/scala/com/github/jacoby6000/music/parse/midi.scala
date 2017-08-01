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
  sealed trait MidiChannelModeEvent extends MidiEvent
  sealed trait SysexEvent extends Event
  sealed trait MetaEvent extends Event

  case class NoteOff(channel: Int, key: Int, velocity: Int) extends MidiVoiceEvent
  case class NoteOn(channel: Int, key: Int, velocity: Int) extends MidiVoiceEvent
  case class KeyPressure(channel: Int, key: Int, pressure: Int) extends MidiVoiceEvent
  case class ControllerChange(channel: Int, controller: Int, value: Int) extends MidiVoiceEvent
  case class ProgramChange(channel: Int, program: Int) extends MidiVoiceEvent
  case class ChannelKeyPressure(channel: Int, pressure: Int) extends MidiVoiceEvent
  case class PitchBend(channel: Int, leastSig: Int, mostSig: Int) extends MidiVoiceEvent

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
  case class UnknownMetaEvent(evType: Int, data: BitVector) extends MetaEvent

  case class Header(chunkType: ChunkType, format: Format, tracks: Int, division: Division)
  case class Track(chunkType: ChunkType, events: List[TrackEvent])
  case class TrackEvent(deltaTime: Int, event: Event)

  val twoUint8 = uint8 ~ uint8

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
    (uint4.withContext("adr") ~ uint4.withContext("chan") ~ bits).exmap[MidiEvent](
      {
        case ((0x8, chan), bs) => twoUint8.withContext("note-off").decodeValue(bs).map(x => NoteOff(chan, x._1, x._2))
        case ((0x9, chan), bs) => twoUint8.withContext("note-on").decodeValue(bs).map(x => NoteOn(chan, x._1, x._2))
        case ((0xA, chan), bs) => twoUint8.withContext("key-pressure").decodeValue(bs).map(x => KeyPressure(chan, x._1, x._2))
        case ((0xC, chan), bs) => uint8.withContext("program-change").decodeValue(bs).map(ProgramChange(chan, _))
        case ((0xD, chan), bs) => uint8.withContext("channel-key-pressure").decodeValue(bs).map(ChannelKeyPressure(chan, _))
        case ((0xE, chan), bs) => twoUint8.withContext("pitch-bend").decodeValue(bs).map(x => PitchBend(chan, x._1, x._2))
        case ((0xB, chan), bs1) =>
          (uint8 ~ bits).withContext("voice").decodeValue(bs1).flatMap {
            case (0x78, bs) => uint8.decodeValue(bs).map(_ => AllSoundOff(chan))
            case (0x79, bs) => uint8.decodeValue(bs).map(_ => ResetAllControllers(chan))
            case (0x7A, bs) => uint8.decodeValue(bs).flatMap {
              case 0x00 => Attempt.successful(LocalControl(chan, false))
              case 0x7F => Attempt.successful(LocalControl(chan, true))
              case other => Attempt.failure(Err(
                s"Unexpected value for LocalControl (0xBn7A) message. " +
                s"Got ${other.toHexString.toUpperCase} expected 0x00 (off) or 0x7F (on)."
              ))
            }
            case (0x7B, bs) => uint8.decodeValue(bs).map(_ => AllNotesOff(chan))
            case (0x7C, bs) => uint8.decodeValue(bs).map(_ => OmniModeOff(chan))
            case (0x7D, bs) => uint8.decodeValue(bs).map(_ => OmniModeOn(chan))
            case (0x7E, bs) => uint8.decodeValue(bs).map(MonoModeOn(chan, _))
            case (0x7F, bs) => uint8.decodeValue(bs).map(_ => PolyModeOn(chan))
            case (k, bs) => uint8.decodeValue(bs).map(v => ControllerChange(chan, k, v))
          }
        case ((adr, _), _) =>
          Attempt.failure(Err(s"Unexpected event type ${adr.toHexString.toUpperCase}"))
      },
      {
        case NoteOff(n, k, v) => twoUint8.encode((k, v)).map(((0x8, n), _))
        case NoteOn(n, k, v) => twoUint8.encode((k, v)).map(((0x9, n), _))
        case KeyPressure(n, k, v) => twoUint8.encode((k, v)).map(((0xA, n), _))
        case ControllerChange(n, k, v) => twoUint8.encode((k, v)).map(((0xB, n), _))
        case ProgramChange(n, k) => uint8.encode(k).map(((0xC, n), _))
        case ChannelKeyPressure(n, k) => uint8.encode(k).map(((0xD, n), _))
        case PitchBend(n, k, v) => twoUint8.encode((k, v)).map(((0xE, n), _))
        case AllSoundOff(n) => Attempt.successful(((0xB, n), hex"0x7800".bits))
        case ResetAllControllers(n) => Attempt.successful(((0xB, n), hex"0x7900".bits))
        case LocalControl(n, false) => Attempt.successful(((0xB, n), hex"0x7A00".bits))
        case LocalControl(n, true) => Attempt.successful(((0xB, n), hex"0x7A7F".bits))
        case AllNotesOff(n) => Attempt.successful(((0xB, n), hex"0x7B00".bits))
        case OmniModeOff(n) => Attempt.successful(((0xB, n), hex"0x7C00".bits))
        case OmniModeOn(n) => Attempt.successful(((0xB, n), hex"0x7D00".bits))
        case MonoModeOn(n, m) => uint8.encode(m).map(x => ((0xB, n), (hex"0x7E" ++ ByteVector(Array(x.toByte(false)))).bits))
        case PolyModeOn(n) => Attempt.successful(((0xB, n), hex"0x7F00".bits))
      }
    ).withContext("midi")
  }

  val sysexEventCodec: Codec[SysexEvent] = {
    (uint8 ~ variableSizeBytes(vint, bytes)).exmap[SysexEvent](
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
    ).withContext("sysex")
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
      ).withContext("smtpe")

    (constant(hex"0xFF") ~ uint8 ~ bits).exmap[MetaEvent](
      {
        case (((), 0x00), bitVec) => uint8.decodeValue(bitVec).map(SequenceNumber(_))
        case (((), 0x01), bitVec) => variableSizeBytes(vint, ascii).decodeValue(bitVec).map(TextEvent(_))
        case (((), 0x02), bitVec) => variableSizeBytes(vint, ascii).decodeValue(bitVec).map(CopyrightNotice(_))
        case (((), 0x03), bitVec) => variableSizeBytes(vint, ascii).decodeValue(bitVec).map(SequenceName(_))
        case (((), 0x04), bitVec) => variableSizeBytes(vint, ascii).decodeValue(bitVec).map(InstrumentName(_))
        case (((), 0x05), bitVec) => variableSizeBytes(vint, ascii).decodeValue(bitVec).map(Lyric(_))
        case (((), 0x06), bitVec) => variableSizeBytes(vint, ascii).decodeValue(bitVec).map(Marker(_))
        case (((), 0x07), bitVec) => variableSizeBytes(vint, ascii).decodeValue(bitVec).map(CuePoint(_))
        case (((), 0x20), bitVec) =>
          (uint8 ~ uint8).decodeValue(bitVec).flatMap {
            case (0x01, cc) => Attempt.successful(MIDIChannelPrefix(cc))
            case (addr, _) => Attempt.failure(Err(
              s"Failed to decode meta event. " +
                s"Got ${addr.toHexString.toUpperCase} expected 0x01 for event 0x20."
            ))
          }
        case (((), 0x2F), bitVec) => uint16.decodeValue(bitVec).map(_ => EndOfTrack)
        case (((), 0x51), bitVec) => uint24.decodeValue(bitVec).map(SetTempo(_))
        case (((), 0x54), bitVec) => smtpeOffsetCodec.decodeValue(bitVec)
        case (((), 0x58), bitVec) =>
          (uint8 ~ uint8 ~ uint8 ~ uint8).decodeValue(bitVec).map {
            case (((nn, dd), cc), bb) => TimeSignature(nn, dd, cc, bb)
          }
        case (((), 0x59), bitVec) =>
          (int8 ~ uint8).decodeValue(bitVec).flatMap {
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

        case (((), adr), bitVec) => Attempt.successful(UnknownMetaEvent(adr, bitVec))
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
        case UnknownMetaEvent(adr, bv) => Attempt.successful((((), adr), bv))
      }
    ).withContext("meta")
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

      def decode(bits: BitVector): Attempt[DecodeResult[Event]] = {
        println(bits)
        sysex.decode(bits) orElse
          meta.decode(bits) orElse
          midi.decode(bits)
      }
    }

  val trackCodec: Codec[Track] =
    (chunkTypeCodec.withContext("chunktype") ::
      variableSizeBytesLong(
        uint32,
        list((vint.withContext("deltatime") ::
          eventCodec.withContext("event")).as[TrackEvent])
      )
    ).withContext("track").as[Track]

  val fileCodec =
    (headerCodec ~ list(trackCodec)).withContext("file")


  implicit class AOps[A](val a: A) extends AnyVal {
    def |>>(f: A => Unit): A = {f(a); a}
  }
}
