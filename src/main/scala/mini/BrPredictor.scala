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

  val prediction_pc: UInt = RegInit(0.U(xlen.W))   // current input PC
  val prediction_addr: UInt = RegInit(0.U(xlen.W)) // current output PC

  io.predicted_address := io.next_pc
  io.hit := !io.br_taken


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
