package com.github.jacoby6000.music.unfolds

import com.github.jacoby6000.maestro.midi.data._
import com.github.jacoby6000.music.core.ast._
import matryoshka._
import matryoshka.implicits._

import scalaz.Scalaz._
import scalaz._

object midi {

  sealed trait UnfoldError
  case class TrackCountMismatch(got: Int, expected: Int) extends UnfoldError
  case class UnexpectedChunk(got: ChunkType, expected: ChunkType) extends UnfoldError

  type MidiFileIgnoreExtension = MidiFile[Nothing, Nothing, Nothing, Nothing]

  type Velocity = Int
  type Key = Int
  type DeltaTime = Int


  //def state[F[_[_]]](implicit corecursive: Corecursive[F[Segment]]): State[UnfoldState, F[Segment]] =
    //State.A

  type Func[I] = MidiFileIgnoreExtension => I
  type Alg[FF[_[_]]] = Func[UnfoldError \/ Segment[FF[Segment] \/ MidiFileIgnoreExtension]]

  def coAlgFile[FF[_[_]]](file: MidiFileIgnoreExtension): Alg[FF] = {
    if (file.tracks.size != file.header.tracks) {
      TrackCountMismatch(file.tracks.size, file.header.tracks).left.pure[Func]
    } else {
      file.header.format match {
        case SingleTrack =>
          if(file.tracks.size != 1) {
            TrackCountMismatch(file.tracks.size, 1).left.pure[Func]
          } else { state =>
            file.tracks match {
              case Seq() => Sequence(List.empty).right
              case vec =>
                val track = vec.head
                track.chunkType match {
                  case MThd => UnexpectedChunk(MThd, MTrk).left
                  case MTrk =>
                   // val noteEvents = bundleNoteEvents(track.events.map(_.event))
                    ???
                  case chnk: UnknownChunkType =>
                    UnexpectedChunk(chnk, MTrk).left
                }
            }
          }
        case _ => ???
      }
    }
  }

  def unfoldMidi[FF[_[_]]](file: MidiFileIgnoreExtension)(implicit corecursive: Corecursive.Aux[FF[Segment], Segment]): UnfoldError \/ FF[Segment] =
    file.apoM[FF[Segment]].apply[UnfoldError \/ ?, Segment](coAlgFile[FF](file))

}
