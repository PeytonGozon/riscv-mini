package mini

import chisel3._
import mini.Control._

class BrPredictorIO(xlen: Int) extends Bundle {

  // Inputs [FETCH]
  var curr_pc: UInt = Input(UInt(xlen.W))

  // Inputs [Execute]
  var br_type: UInt = Input(UInt(3.W))     // The same as in BrCond
  var br_taken: Bool = Input(Bool())       // Whether the branch was taken or not.
  var br_addr: UInt = Input(UInt(xlen.W))  // The addressed branched to (if the branch is taken).

  // Outputs
  var pred_addr: UInt = Output(UInt(xlen.W))  // The address predicted
  var pred_made: Bool = Output(Bool())        // Whether a prediction was made or not
  var pred_wrong_addr: Bool = Output(Bool())  // Whether the predicted address was correct or incorrect.
  var pred_incorrect: Bool = Output(Bool())   // Whether the prediction to take the branch was correct or incorrect.
}

trait BrPredictor extends Module {
  def xlen: Int
  val io: BrPredictorIO
}

class BrPredictorOneEntry(val xlen: Int) extends BrPredictor {
  val io: BrPredictorIO = IO(new BrPredictorIO(xlen))

  // The branch prediction table
  val cached_pc_write: UInt = RegInit(0.U(xlen.W))
  val cached_pc_read: UInt = RegNext(cached_pc_write)
  val cached_addr_write: UInt = RegInit(0.U(xlen.W))
  val cached_addr_read: UInt = RegNext(cached_addr_write)

  val hasUpdatedTableOnce: Bool = RegInit(false.B)

  // Whether the last instruction was a branch [EXECUTE]
  val was_br_inst: Bool = io.br_type =/= BR_XXX

  // The value of the PC in the execute stage.
  // this is a delayed value of curr_pc by one clock tick to be used to update the internals.
  val last_pc: UInt = RegNext(io.curr_pc)

  /**
    * The Prediction
    * Note: pred_addr will need to change when moving to a table.
    */
  io.pred_made := io.curr_pc === cached_pc_read && hasUpdatedTableOnce  // TODO: need a way to ensure that this won't occur on cycle 1.
  io.pred_addr := cached_addr_read

  /**
    * Update the table (delayed by 1 clock tick from the prediction)
    */
  when (was_br_inst) {
    // Whether the last instruction is in the table or not.
    when (last_pc === cached_pc_read) {

      when (io.br_taken) {

        // We always predict that the branch is taken, so if the branch is taken, then the prediction was "correct"
        // (as in, we predicted the branch was taken, and it was taken).
        // The address predicted *could be wrong* however.
        io.pred_incorrect := false.B
        io.pred_wrong_addr := io.br_addr === cached_addr_read

        // Update the cached address when the predicted address was incorrect.
        when (io.br_addr =/= cached_addr_read) {
          cached_addr_write := io.br_addr
        }

      } otherwise {
        io.pred_incorrect := true.B
        io.pred_wrong_addr := false.B
      }

    } otherwise {
      io.pred_incorrect := false.B
      io.pred_wrong_addr := false.B

      when (io.br_taken) {
        // Replacement Policy
        cached_pc_write := last_pc
        cached_addr_write := io.br_addr

        hasUpdatedTableOnce := true.B
      } otherwise {
        // Do nothing -- didn't predict on the instruction & the instruction wasn't taken.
      }
    }

  } otherwise {
    io.pred_incorrect := false.B
    io.pred_wrong_addr := false.B
  }

}



 class BrPredictor2IO(xlen: Int) extends Bundle {

   /**
     * "System 1" is responsible for when the current PC is a branch instruction
     */
   // System 1 Inputs
   var br_type: UInt = Input(UInt(3.W))  // Directly the same as in BrCond.
   var curr_pc: UInt = Input(UInt(xlen.W))  // The PC to predict on. (latched, FE PC!)

   // System 1 Outputs
   var pred_made: Bool = Output(Bool())  // Whether a prediction was just made or not
   var pred_addr: UInt = Output(UInt(xlen.W))  // The predicted address

   /**
     * "System 2" is responsible for checking the branch predictor's work on the clock cycle after a prediction is made.
     */
   // System 2 Inputs
   var br_taken: Bool = Input(Bool())
   var br_addr: UInt = Input(UInt(xlen.W))

   // System 2 Output(s)
   var pred_incorrect: Bool = Output(Bool())  // Whether the last prediction was correct or not (alt: whether to bubble a NOP through the pipeline or not).

   var cached_pc: UInt = Output(UInt(xlen.W))

 }

trait BrPredictor2 extends Module {
  def xlen: Int
  val io: BrPredictor2IO
}

class BrPredictorStatic(val xlen: Int) extends BrPredictor2 {
  val io: BrPredictor2IO = IO(new BrPredictor2IO(xlen))

  /**
    * Non-register values.
    */
  val is_br_inst: Bool = io.br_type =/= BR_XXX

  // Should these be RegNext's? Concern: if the table gets updated, then we still want it to be a misprediction.
  /**
    * The Branch Predictor Table (stateful data)
    */

  val cached_pc_write: UInt = RegInit(0.U(xlen.W))  // The cached branch PC
  val cached_pc_read: UInt = RegNext(cached_pc_write)

  val cached_addr_write: UInt = RegInit(0.U(xlen.W))  // The address the cached branch goes to.
  val cached_addr_read: UInt = RegNext(cached_addr_write)

  io.cached_pc := cached_pc_read



  when (is_br_inst) {

    when (io.curr_pc === cached_pc_read) {
      // We have a branch instruction, and the PC is in the table.

      // Therefore, we make a prediction.
      // io.pred_addr := cached_addr
      io.pred_made := true.B

      when (io.br_taken) {
        // We predicted that the branch was taken, and it was! We now only must ensure that the address was correct.

        io.pred_incorrect := io.br_addr =/= cached_addr_read

        when (io.br_addr === cached_addr_read) {
          // Do nothing! Successful prediction!
        } otherwise {
          // Prediction was incorrect -- and the cached address needs to be updated!
          cached_addr_write := io.br_addr
        }

      } otherwise {
        // We predicted that the branch was taken, but it was not. Miss-predict!
        io.pred_incorrect := true.B
      }

    } otherwise {  // We had a branch instruction, but the PC was not in the table.
      // Consequently, there was no prediction (and the prediction was not incorrect).
      io.pred_made := false.B
      io.pred_incorrect := false.B

      when (io.br_taken) {
        // If the branch was ultimately taken, then invoke the Replacement Policy.
        cached_pc_write := io.curr_pc
        cached_addr_write := io.br_addr
      } otherwise {
        // Do nothing
      }
    }

  } otherwise {
    io.pred_made := false.B
    io.pred_incorrect := false.B
  }

  // Prediction address is always the cached address.
  io.pred_addr := cached_addr_read

}

/**
  * Idea: take in a branch instruction, return the instruction from the last time it was executed.
  *
  * If the instruction isn't a branch, pass it through unchanged?
  */
//class BrPredictorIO(xlen: Int) extends Bundle {
//  // Inputs to do the work
//  var current_pc: UInt = Input(UInt(xlen.W))
//  var next_pc: UInt = Input(UInt(xlen.W))
//
//  // Inputs to check our work
//  var br_taken: Bool = Input(Bool())
//  var br_address: UInt = Input(UInt(xlen.W))
//  var fe_pc: UInt = Input(UInt(xlen.W))
//
//  // Outputs
//  var predicted_address: UInt = Output(UInt(xlen.W))
//  var hit: Bool = Output(Bool())
//}
//
//trait BrPredictor extends Module {
//  def xlen: Int
//  val io: BrPredictorIO
//}
//
//class BrPredictorStaticAlwaysTake(val xlen: Int) extends BrPredictor {
//  val io: BrPredictorIO = IO(new BrPredictorIO(xlen))
//
//  // make variables for whether we just predicted or not.
//  // tbh, simplify all the weird logic steps.
//
//  val prediction_pc: UInt = RegInit(0.U(xlen.W))   // current input PC
//  val prediction_addr: UInt = RegInit(0.U(xlen.W)) // current output PC
//  val just_predicted: Bool = RegNext(prediction_pc === io.fe_pc) // whether we just predicted accurately or not.
//
//  /** This makes BrPredictor act like an identity gate
//    */
////  io.predicted_address := io.next_pc
////  io.hit := !io.br_takenA
//
//  /** Update internal state whenever we didn't predict OR we mispredicted
//    * We know when we mispredicted by: 1. branch was taken; 2. last stored branch inst PC ≠ latched branch inst PC
//    *
//    * Note: We have to wait for io.br_taken to be true to know that there was an actual branch taken.
//    */
//  when(io.br_taken && (prediction_pc =/= io.fe_pc || prediction_addr =/= io.br_address)) {
//    prediction_pc := io.fe_pc
//    prediction_addr := io.br_address
//  }
//
//  /** Start predicting
//    * 1. If the current PC is the prediction PC --> give the prediction address
//    * 2. If we don't recognize the current PC   --> give the next PC.
//    */
//  io.predicted_address := MuxCase(
//    io.next_pc,
//    IndexedSeq(
//      (io.current_pc === prediction_pc) -> prediction_addr
//    )
//  )
//
//  /** Did we do a good job predicting? If not: insert that NOP
//    *
//    *   When doesn't the branch predictor act?
//    *     - Whenever there was no branch instruction
//    *
//    *   How do we know when there wasn't a branch instruction?
//    *     -
//    */
//
//  // If we didn't take a branch, call that a hit (good -- no cause for concern)
//  // If we did take the branch, ensure that our predicted address was equal to the branching address.
//  io.hit := !io.br_taken || (prediction_addr === io.br_address)
//
//  // If we took the branch, and the branch was not currently in the predictor
//  // Update the predictor
////  when(io.br_taken && prediction_pc =/= io.fe_pc) {
////    prediction_pc := io.fe_pc
////    prediction_addr := io.next_pc
////  }
////
////  io.predicted_address := MuxCase(
////    io.next_pc,
////    IndexedSeq(
////      (io.current_pc === prediction_pc) -> prediction_addr
////    )
////  )
////
////  io.hit := !just_predicted // !io.br_taken || (prediction_pc === io.br_address)
//
//
//  /**
//    * Assumption: a "miss" => we flush the pipeline with a NOP in the exceptional mux
//    *
//    * We have a hit (correct prediction) if:
//    *   1. the branch was taken & our prediction was correct
//    *   2.
//    *
//    * We have a miss (incorrect prediction) if:
//    *   1. the branch was taken & our prediction was incorrect
//    *   2. the branch was not taken
//    *
//    */
////  io.hit := !(io.br_taken && io.br_address =/= prediction_addr)
//
//  /**
//    * The "predicted address" (output) of this branch predictor is:
//    *   1. If the addresses is in the predictor table, then output the corresponding prediction address
//    *   2. If the addresses is not in the predictor table, then output the next PC
//    *   3. If we mis-predicted, send along the latched PC (fe_pc) + 4
//    */
////  io.predicted_address := MuxCase(
////    prediction_addr,
////    IndexedSeq(
////      (io.current_pc =/= prediction_pc) -> io.next_pc,
////      (io.br_taken === false.B) -> (io.fe_pc + 4.U),
////    )
////  )
//
//  /**
//    * If the branch was taken, there are a few potential updates
//    *
//    * 1. If the branch's PC, now stored in the FE_PC, is not the prediction PC -- we must update it & the prediction addr
//    * 2. If the branch PC matches, but the prediction address was wrong, update the prediction address
//    */
////  when(io.br_taken) {
////    // Update internal state of the branch predictor if the branch was taken, and the branch predictor didn't contain
////    // the
////    when(prediction_pc =/= io.fe_pc) {
////      prediction_pc := io.fe_pc
////      prediction_addr := io.br_address
////    }
////
////    when(prediction_addr =/= io.br_address) {
////      prediction_addr := io.br_address
////    }
////  }
//
//  /**
//    * Update the prediction address
//    *   1. This only happens if we took the branch, but predicted the incorrect address
//    */
////  when(io.br_taken && (io.br_address =/= prediction_addr)) {
////    prediction_addr := io.br_address
////  }
//
//}

// All from design 2.0 (Jan 4)
//
//  when (is_br_inst) {
//
//    /**
//      * System 1 -- "Making the prediction"
//      *  - If we have a branch instruction & the current PC is in the table,
//      *      then make the prediction! (set the prediction address & prediction flag)
//      *  - If the current PC is not in the table, ensure the prediction flag is false.
//      */
//    when (io.curr_pc === cached_pc) {
//      io.pred_addr := cached_addr
//      io.pred_made := true.B
//    } otherwise {
//      io.pred_made := false.B
//    }
//
//    /**
//      * System 2 -- "clean up"
//      * After a prediction was made, we need to ensure the prediction was accurate -- and act correspondingly.
//      *   - If the prediction was correct, then nothing needs to happen.
//      *   - If the prediction was incorrect, the system will need to cancel the computation.
//      *   - If the branch was taken but the address was not in the table, then the table needs to be updated.
//      *   - If the branch was not taken and the address was not in the table, then nothing needs to occur.
//      */
//    when (io.br_addr === cached_addr) {
//      // If the branch address is the same as the cached address, the
//      // prediction was correct if the branch was taken. Otherwise, it was incorrect.
//      io.incorrect_pred := !io.br_taken
//    } otherwise {
//      when (io.br_taken) {
//        cached_pc :=
//      }
//    }
//
//  } otherwise {
//    io.incorrect_pred := false.B
//    io.pred_made := false.B
//  }
//
//
//  when (check_and_update) {
//    when (prev_pred_pc === cached_pc) {
//      when (io.br_taken) {  // When the prediction was correct
//        io.incorrect_pred := false.B
//      } otherwise {
//        // If the prediction was incorrect, cancel the computation.
//        io.incorrect_pred := true.B
//      }
//    } otherwise {
//      when (io.br_taken) {
//        // If the branch was not in the table & it was taken, update the table. Note: no prediction was made.
//        cached_pc := prev_pred_pc
//        cached_addr := io.br_addr
//        io.incorrect_pred := false.B
//      } otherwise {
//        // Do Nothing
//        io.incorrect_pred := false.B
//      }
//    }
//  } otherwise {
//    // If no need to check and update, ensure that all outputs reflect that.
//    io.incorrect_pred := false.B
//  }