package com.github.jacoby6000.music.parse

import scodec.bits._
import scodec.codecs._
import scodec.{bits => _, codecs => _, _}
import shapeless._

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
        case Inr(Inl((f1, _))) => TicksPerQuarter(f1)
        case Inr(Inr(_)) => throw new Exception("This is literally actually impossible.")
      },
      {
        case Frames(f1, f2) => Coproduct[CoPro](((f1, -f2), ()))
        case TicksPerQuarter(n) => Coproduct[CoPro](((n >> 8, n), ()))
      }
    ).choice
  }

  def midiEventCodec: Codec[Codec[MidiEvent]] = {
    def threeParamCodec[A](f: (Int, Int, Int) => A)(g: A => (Int, Int, Int)) =
      (uint4 ~ twoUint8).xmap[A](
        x => f(x._1, x._2._1, x._2._2),
        a => { val result = g(a); (result._1, (result._2, result._3))}
      )

    def twoParamCodec[A](f: (Int, Int) => A)(g: A => (Int, Int)) =
      (uint4 ~ uint8).xmap[A](
        x => f(x._1, x._2),
        a => { val result = g(a); (result._1, result._2)}
      )

    val noteOffCodec = threeParamCodec(NoteOff)(NoteOff.unapply(_).get)
    val noteOnCodec = threeParamCodec(NoteOn)(NoteOn.unapply(_).get)
    val keyPressureCodec = threeParamCodec(KeyPressure)(KeyPressure.unapply(_).get)
    val programChangeCodec = twoParamCodec(ProgramChange)(ProgramChange.unapply(_).get)
    val channelKeyPressureCodec = twoParamCodec(ChannelKeyPressure)(ChannelKeyPressure.unapply(_).get)
    val pitchBendCodec = threeParamCodec(PitchBend)(PitchBend.unapply(_).get)
    val controllerChangeCodec = threeParamCodec(ControllerChange)(ControllerChange.unapply(_).get)

    val channelModeCodecImpl: Codec[MidiChannelModeEvent] =
      uint8.consume({ chan =>

        def channelModeEventCodec[A](f: Int => A)(g: A => Int) = uint8.xmap(f, g)

        // creates a codec for As where the byte is expected to be zero (0x00).
        def channelModeEventCodec0byte[A](f: => A) =
          channelModeEventCodec(_ => f)(_ => 0x00)

        val allSoundOffCodec = channelModeEventCodec0byte(AllSoundOff(chan))
        val resetAllControllersCodec = channelModeEventCodec0byte(ResetAllControllers(chan))
        val allNotesOffCodec = channelModeEventCodec0byte(AllNotesOff(chan))
        val omniModeOffCodec = channelModeEventCodec0byte(OmniModeOff(chan))
        val omniModeOnCodec = channelModeEventCodec0byte(OmniModeOn(chan))
        val monoModeOnCodec = channelModeEventCodec(MonoModeOn(chan, _))(_.channels)
        val polyModeOnCodec = channelModeEventCodec0byte(PolyModeOn(chan))
        val localControlCodec = channelModeEventCodec[LocalControl]({
            case 0x00 => LocalControl(chan, false)
            case _ => LocalControl(chan, true)
          })({
            case LocalControl(_, false) => 0x00
            case LocalControl(_, true) => 0x7F
          })

        discriminated[MidiChannelModeEvent]
          .by(uint8)
          .typecase(0x78, allSoundOffCodec.withContext("all-sound-off"))
          .typecase(0x79, resetAllControllersCodec.withContext("reset-all-controllers"))
          .typecase(0x7A, localControlCodec.withContext("local-control"))
          .typecase(0x7B, allNotesOffCodec.withContext("all-notes-off"))
          .typecase(0x7C, omniModeOffCodec.withContext("omni-mode-off"))
          .typecase(0x7D, omniModeOnCodec.withContext("omni-mode-on"))
          .typecase(0x7E, monoModeOnCodec.withContext("mono-mode-on"))
          .typecase(0x7F, polyModeOnCodec.withContext("poly-mode-on"))
          .withContext("channel-mode")
      })({
        case AllSoundOff(_) => 0x78
        case ResetAllControllers(_) => 0x79
        case AllNotesOff(_) => 0x7B
        case OmniModeOff(_) => 0x7C
        case OmniModeOn(_) => 0x7D
        case MonoModeOn(_, _) => 0x7E
        case PolyModeOn(_) => 0x7F
        case LocalControl(_, _) => 0x7A
      })

    val controllerChangeOrChannelModeCodec =
      fallback(
        controllerChangeCodec.withContext("controller-change"),
        channelModeCodecImpl.withContext("channel-mode")
      ).exmapc(e => Attempt.successful(e.merge)){
        case mode: MidiChannelModeEvent => Attempt.successful(Right(mode))
        case change: ControllerChange => Attempt.successful(Left(change))
        case ev => Attempt.failure(Err(
          s"Unexpected event type. Expected MidiChannelModeEvent or ControllerChange, got $ev."
        ))
      }


    discriminated[Codec[MidiEvent]]
      .by(uint4)
      .typecase(0x8, provide(noteOffCodec.withContext("note-off").upcast[MidiEvent]))
      .typecase(0x9, provide(noteOnCodec.withContext("note-on").upcast[MidiEvent]))
      .typecase(0xA, provide(keyPressureCodec.withContext("key-pressure").upcast[MidiEvent]))
      .typecase(0xB, provide(controllerChangeOrChannelModeCodec.upcast[MidiEvent]))
      .typecase(0xC, provide(programChangeCodec.withContext("program-change").upcast[MidiEvent]))
      .typecase(0xD, provide(channelKeyPressureCodec.withContext("channel-key-pressure").upcast[MidiEvent]))
      .typecase(0xE, provide(pitchBendCodec.withContext("pitch-bend").upcast[MidiEvent]))
      .withContext("midi")
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
    val uint8or24: Codec[Int] = new Codec[Int] {
      def sizeBound = SizeBound(uint8.sizeBound.lowerBound, uint24.sizeBound.upperBound)
      def encode(i: Int) =
        uint8.encode(i) orElse uint24.encode(i)
      def decode(b: BitVector) =
        uint24.decode(b) orElse uint8.decode(b)
    }

    val smtpeOffsetCodec =
      (uint16 ~ uint16 ~ uint16 ~ uint16 ~ uint16).xmap[SMTPEOffset](
        {
          case ((((h, m), s), fr), ff) => SMTPEOffset(h, m, s, fr, ff)
        },
        {
          case SMTPEOffset(h, m, s, fr, ff) => ((((h, m), s), fr), ff)
        }
      ).withContext("smtpe")

    def variable[A](codec: Codec[A]) = variableSizeBytes(vint, codec)

    val sequenceNumberCodec = variable(uint8).xmapc(SequenceNumber)(_.sequenceNumber)
    val textEventCodec = variable(ascii).xmapc(TextEvent)(_.text)
    val copyrightNoticeCodec = variable(ascii).xmapc(CopyrightNotice)(_.text)
    val sequenceNameCodec = variable(ascii).xmapc(SequenceName)(_.text)
    val instrumentNameCodec = variable(ascii).xmapc(InstrumentName)(_.text)
    val lyricCodec = variable(ascii).xmapc(Lyric)(_.text)
    val markerCodec = variable(ascii).xmapc(Marker)(_.text)
    val cuePointCodec = variable(ascii).xmapc(CuePoint)(_.text)
    val midiChannelPrefixCodec =
      variable(uint8 ~ uint8).exmapc({
        case (0x01, cc) => Attempt.successful(MIDIChannelPrefix(cc))
        case (addr, _) => Attempt.failure(Err(
          s"Failed to decode meta event. " +
            s"Got ${addr.toHexString.toUpperCase} expected 0x01 for event 0x20."
        ))
      })(x => Attempt.successful((0x01, x.channel)))
    val endOfTrackCodec = uint8.xmapc(_ => EndOfTrack)(_ => 0x00)
    val setTempoCodec = variable(uint24).xmapc(SetTempo)(_.tempo)
    val timeSignatureCodec =
      variable(uint8 ~ uint8 ~ uint8 ~ uint8).xmapc({
        case (((nn, dd), cc), bb) => TimeSignature(nn, dd, cc, bb)
      })(x => (((x.numerator, x.denominator), x.clocksPerMetronome), x.thirtySecondNotesPerTwentyFourMidiClocks))
    val keySignatureCodec =
      variable(int8 ~ uint8).exmapc({
        case (sf, 0) => Attempt.successful(KeySignature(sf, false))
        case (sf, 1) => Attempt.successful(KeySignature(sf, true))
        case (_, x) => Attempt.failure(Err(
          s"Invalid input for Meta Event 0x59. Got $x for major/minor mode, expected 0 or 1."
        ))
      })(k => Attempt.successful((k.numAccidentals, if(k.minor) 1 else 0)))
    val sequencerSpecificMetaEventCodec =
      variable(uint8or24 ~ bytes).xmapc({
        case (n, leftover) => SequencerSpecificMetaEvent(n, leftover)
      })(x => (x.id, x.data))

    // fallback
    val unknownMetaEvent =
      (constant(hex"0xFF") ~ uint8 ~ variable(bits)).xmapc({
        case (((), adr), bs) => UnknownMetaEvent(adr, bs)
      })(ev => (((), ev.evType), ev.data))



    discriminatorFallback(unknownMetaEvent,
      discriminated[MetaEvent]
        .by(constant(hex"0xFF") ~ uint8)
        .typecase(((), 0x00), sequenceNumberCodec.withContext("sequence-number"))
        .typecase(((), 0x01), textEventCodec.withContext("text-event"))
        .typecase(((), 0x02), copyrightNoticeCodec.withContext("copyright-notice"))
        .typecase(((), 0x03), sequenceNameCodec.withContext("sequence-name"))
        .typecase(((), 0x04), instrumentNameCodec.withContext("instrument-name"))
        .typecase(((), 0x05), lyricCodec.withContext("lyric"))
        .typecase(((), 0x06), markerCodec.withContext("marker"))
        .typecase(((), 0x07), cuePointCodec.withContext("cue-point"))
        .typecase(((), 0x20), midiChannelPrefixCodec.withContext("midi-channel-prefix"))
        .typecase(((), 0x2F), endOfTrackCodec.withContext("end-of-track"))
        .typecase(((), 0x51), setTempoCodec.withContext("set-tempo"))
        .typecase(((), 0x54), smtpeOffsetCodec.withContext("smtpe-offset"))
        .typecase(((), 0x58), timeSignatureCodec.withContext("time-signature"))
        .typecase(((), 0x59), keySignatureCodec.withContext("key-signature"))
        .typecase(((), 0x7F), sequencerSpecificMetaEventCodec.withContext("sequencer-specific-meta-event"))
    ).xmapc(_.merge)({
      case uk: UnknownMetaEvent => Left(uk)
      case e => Right(e)
    }).withContext("meta")
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
      val midi = new Codec[MidiEvent] {
        var previousCodec: Option[Codec[MidiEvent]] = None
        var previousChannel: BitVector = BitVector(Array(0.toByte))

        override def decode(bits: BitVector): Attempt[DecodeResult[MidiEvent]] =
          midiEventCodec.decode(bits).flatMap { result =>
            previousCodec = Some(result.value)
            previousChannel = bits.drop(4).take(4)
            result.value.decode(result.remainder)
          } recoverWith {
            case err =>
              previousCodec.map(_.decode(previousChannel ++ bits)).getOrElse(Attempt.failure(err))
          }

        override def encode(value: MidiEvent): Attempt[BitVector] =
          midiEventCodec.encode(provide(value))

        override def sizeBound: SizeBound = SizeBound(2, None)
      }.upcast[Event]

      def sizeBound: SizeBound = SizeBound(12, None)

      def encode(value: Event): Attempt[BitVector] =
        sysex.encode(value) orElse
          meta.encode(value) orElse
          midi.encode(value)

      def decode(bits: BitVector): Attempt[DecodeResult[Event]] = {
        sysex.decode(bits) orElse
          meta.decode(bits) orElse
          midi.decode(bits)
      }
    }

  val trackEventCodec =
    (vint.withContext("deltatime") :: eventCodec.withContext("event")).as[TrackEvent]

  val trackCodec: Codec[Track] =
    (chunkTypeCodec.withContext("chunktype") ::
      variableSizeBytesLong(
        uint32,
        list(trackEventCodec)
      )
    ).withContext("track").as[Track]

  val fileCodec =
    (headerCodec ~ list(trackCodec)).withContext("file")

  implicit class CodecOps[A](val codec: Codec[A]) extends AnyVal {
    def debug: Codec[A] = codec.xmap(_ |>> println, _ |>> println)
    //def peekRemaining: Codec[A] = (peek(bits) :: codec)
  }

  implicit class AOps[A](val a: A) extends AnyVal {
    def |>>(f: A => Unit): A = {f(a); a}
  }
}
