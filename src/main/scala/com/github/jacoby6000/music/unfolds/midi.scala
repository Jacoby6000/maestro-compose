package com.github.jacoby6000.music.unfolds

import com.github.jacoby6000.maestro.midi.data._
import com.github.jacoby6000.music.core.ast._
import matryoshka.implicits._
import matryoshka._

import scalaz.{State, Traverse}
import scalaz._
import Scalaz._

object midi {

  sealed trait UnfoldError
  case class TrackCountMismatch(got: Int, expected: Int) extends UnfoldError
  case class UnexpectedChunk(got: ChunkType, expected: ChunkType) extends UnfoldError

  type MidiFileIgnoreExtension = MidiFile[Nothing, Nothing, Nothing, Nothing]

  type Velocity = Int
  type Key = Int
  type DeltaTime = Int


  case class UnfoldState(file: MidiFileIgnoreExtension, pressedKeys: Map[Key, (Velocity, DeltaTime)])

  //def state[F[_[_]]](implicit corecursive: Corecursive[F[Segment]]): State[UnfoldState, F[Segment]] =
    //State.

  def coAlgFile(file: MidiFileIgnoreExtension): UnfoldState => Either[UnfoldError, Segment[UnfoldState]] = {
    if (file.tracks.size != file.header.tracks) {
      Left(TrackCountMismatch(file.tracks.size, file.header.tracks)).pure[UnfoldState => ?]
    } else {
      file.header.format match {
        case SingleTrack =>
          if(file.tracks.size != 1) {
            Left(TrackCountMismatch(file.tracks.size, 1)).pure[UnfoldState => ?]
          } else { state =>
            state.file.tracks match {
              case Seq() => Right(Notes(List.empty))
              case vec =>
                val track = vec.head
                track.chunkType match {
                  case MThd => Left(UnexpectedChunk(MThd, MTrk))
                  case MTrk =>
                    track.
                }
            }
          }
      }
    }
  }

  def coAlgEvents(events: List[Event[Nothing, Nothing, Nothing, Nothing]]): 


  def unfoldMidi[F[_[_]], G[_]: Traverse](file: MidiFileIgnoreExtension)(implicit corecursive: Corecursive.Aux[F[Segment], G]) =
    UnfoldState(file, Map.empty).anaM[F[Segment]](coAlg(file))
}
