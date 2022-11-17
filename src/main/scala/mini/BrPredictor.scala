package mini

import chisel3._
import chisel3.util.MuxCase
import mini.Control._

/**
  * Idea: take in a branch instruction, return the instruction from the last time it was executed.
  *
  * If the instruction isn't a branch, pass it through unchanged?
  */
class BrPredictorIO(xlen: Int) extends Bundle {
  // Inputs to do the work
  var current_pc: UInt = Input(UInt(xlen.W))
  var next_pc: UInt = Input(UInt(xlen.W))

  // Inputs to check our work
  var br_taken: Bool = Input(Bool())
  var br_address: UInt = Input(UInt(xlen.W))
  var fe_pc: UInt = Input(UInt(xlen.W))

  // Outputs
  var predicted_address: UInt = Output(UInt(xlen.W))
  var hit: Bool = Output(Bool())
}

trait BrPredictor extends Module {
  def xlen: Int
  val io: BrPredictorIO
}

class BrPredictorStaticAlwaysTake(val xlen: Int) extends BrPredictor {
  val io: BrPredictorIO = IO(new BrPredictorIO(xlen))

  // make variables for whether we just predicted or not.
  // tbh, simplify all the weird logic steps.

  val prediction_pc: UInt = RegInit(0.U(xlen.W))   // current input PC
  val prediction_addr: UInt = RegInit(0.U(xlen.W)) // current output PC
  val just_predicted: Bool = RegNext(prediction_pc === io.fe_pc) // whether we just predicted accurately or not.

  /** This makes BrPredictor act like an identity gate
    */
//  io.predicted_address := io.next_pc
//  io.hit := !io.br_takenA

  /** Update internal state whenever we didn't predict OR we mispredicted
    * We know when we mispredicted by: 1. branch was taken; 2. last stored branch inst PC ≠ latched branch inst PC
    *
    * Note: We have to wait for io.br_taken to be true to know that there was an actual branch taken.
    */
  when(io.br_taken && (prediction_pc =/= io.fe_pc || prediction_addr =/= io.br_address)) {
    prediction_pc := io.fe_pc
    prediction_addr := io.br_address
  }

  /** Start predicting
    * 1. If the current PC is the prediction PC --> give the prediction address
    * 2. If we don't recognize the current PC   --> give the next PC.
    */
  io.predicted_address := MuxCase(
    io.next_pc,
    IndexedSeq(
      (io.current_pc === prediction_pc) -> prediction_addr
    )
  )

  /** Did we do a good job predicting? If not: insert that NOP
    *
    *   When doesn't the branch predictor act?
    *     - Whenever there was no branch instruction
    *
    *   How do we know when there wasn't a branch instruction?
    *     -
    */

  // If we didn't take a branch, call that a hit (good -- no cause for concern)
  // If we did take the branch, ensure that our predicted address was equal to the branching address.
  io.hit := !io.br_taken || (prediction_addr === io.br_address)

  // If we took the branch, and the branch was not currently in the predictor
  // Update the predictor
//  when(io.br_taken && prediction_pc =/= io.fe_pc) {
//    prediction_pc := io.fe_pc
//    prediction_addr := io.next_pc
//  }
//
//  io.predicted_address := MuxCase(
//    io.next_pc,
//    IndexedSeq(
//      (io.current_pc === prediction_pc) -> prediction_addr
//    )
//  )
//
//  io.hit := !just_predicted // !io.br_taken || (prediction_pc === io.br_address)


  /**
    * Assumption: a "miss" => we flush the pipeline with a NOP in the exceptional mux
    *
    * We have a hit (correct prediction) if:
    *   1. the branch was taken & our prediction was correct
    *   2.
    *
    * We have a miss (incorrect prediction) if:
    *   1. the branch was taken & our prediction was incorrect
    *   2. the branch was not taken
    *
    */
//  io.hit := !(io.br_taken && io.br_address =/= prediction_addr)

  /**
    * The "predicted address" (output) of this branch predictor is:
    *   1. If the addresses is in the predictor table, then output the corresponding prediction address
    *   2. If the addresses is not in the predictor table, then output the next PC
    *   3. If we mis-predicted, send along the latched PC (fe_pc) + 4
    */
//  io.predicted_address := MuxCase(
//    prediction_addr,
//    IndexedSeq(
//      (io.current_pc =/= prediction_pc) -> io.next_pc,
//      (io.br_taken === false.B) -> (io.fe_pc + 4.U),
//    )
//  )

  /**
    * If the branch was taken, there are a few potential updates
    *
    * 1. If the branch's PC, now stored in the FE_PC, is not the prediction PC -- we must update it & the prediction addr
    * 2. If the branch PC matches, but the prediction address was wrong, update the prediction address
    */
//  when(io.br_taken) {
//    // Update internal state of the branch predictor if the branch was taken, and the branch predictor didn't contain
//    // the
//    when(prediction_pc =/= io.fe_pc) {
//      prediction_pc := io.fe_pc
//      prediction_addr := io.br_address
//    }
//
//    when(prediction_addr =/= io.br_address) {
//      prediction_addr := io.br_address
//    }
//  }

  /**
    * Update the prediction address
    *   1. This only happens if we took the branch, but predicted the incorrect address
    */
//  when(io.br_taken && (io.br_address =/= prediction_addr)) {
//    prediction_addr := io.br_address
//  }

}
