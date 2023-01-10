package mini

import Chisel.testers.BasicTester
import chisel3._
import chisel3.util.Counter
import chiseltest.ChiselScalatestTester
import chiseltest.formal.Formal
import org.scalatest.flatspec.AnyFlatSpec

class BrPredictorTestNoBranches(bp: => BrPredictor) extends BasicTester with TestUtils {

  import Control._
  val dut = Module(bp)
  val ctrl = Module(new Control)

  override val insts = Seq.fill(3)(nop)

  val predictIncorrectTake: Vec[Bool] = VecInit(Seq.fill(insts.size)(false.B))
  val predictIncorrectAddress: Vec[Bool] = VecInit(Seq.fill(insts.size)(false.B))
  val madePrediction: Vec[Bool] = VecInit(Seq.fill(insts.size)(false.B))
  val predictedAddress: Vec[UInt] = VecInit(Seq.fill(insts.size)(0.U))

  val (cntr, done) = Counter(true.B, insts.size)

  // Wire up the device
  ctrl.io.inst := VecInit(insts)(cntr)
  dut.io.br_type := ctrl.io.br_type
  dut.io.br_taken := false.B
  dut.io.br_addr := 0.U(32)  // TODO: figure out a better br_addr test. None of this matters for this test.
  dut.io.curr_pc := cntr  // TODO: Figure out a good test bench for this

  when (done) { stop() }

  assert(dut.io.pred_made === madePrediction(cntr))
  assert(dut.io.pred_addr === predictedAddress(cntr))
  assert(dut.io.pred_incorrect === predictIncorrectTake(cntr))
  assert(dut.io.pred_wrong_addr === predictIncorrectAddress(cntr))

  printf(
    "[BrPred] Counter:\t%d, BrInst:\t%b, PredMade:\t%b, PredictedAddr:\t0x%x, MissPredict:\t%b, IncorrectAddr: %b\n",
    cntr,
    (dut.io.br_type =/= BR_XXX),
    dut.io.pred_made,
    dut.io.pred_addr,
    dut.io.pred_incorrect,
    dut.io.pred_wrong_addr
  )

}

class BrPredictorTestOneBranch(bp: => BrPredictor) extends BasicTester with TestUtils {
  import Control._
  val dut = Module(bp)
  val ctrl = Module(new Control)
  val brCond = Module(new BrCondSimple(32))

  // Testing BEQ with no predictions.
  // First cycle should update the branch predictor table.
  val rs1s = Seq(0, 1, 0)
  val rs2s = Seq(0, 0, 1)
  val address = 100

  // Predicts to 100.U if it works
  override val insts = rs1s.zip(rs2s).map(data => B(Funct3.BEQ, data._1, data._2, address))


  // No changes
  val predictIncorrectTake: Vec[Bool] = VecInit(Seq.fill(insts.size)(false.B))
  val predictIncorrectAddress: Vec[Bool] = VecInit(Seq.fill(insts.size)(false.B))
  val madePrediction: Vec[Bool] = VecInit(Seq.fill(insts.size)(false.B))
  val predictedAddress: Vec[UInt] = VecInit(Seq.fill(insts.size)(0.U))

  val (cntr, done) = Counter(true.B, insts.size)

  // Wire up the BrCond
//  brCond.io.rs1 := rs1s(cntr).U(32)

  // Wire up the device
  ctrl.io.inst := VecInit(insts)(cntr)
  dut.io.br_type := ctrl.io.br_type
  dut.io.br_taken := false.B
  dut.io.br_addr := 0.U(32)
  dut.io.curr_pc := cntr

  when (done) { stop() }

  assert(dut.io.pred_made === madePrediction(cntr))
  assert(dut.io.pred_addr === predictedAddress(cntr))
  assert(dut.io.pred_incorrect === predictIncorrectTake(cntr))
  assert(dut.io.pred_wrong_addr === predictIncorrectAddress(cntr))

  printf(
    "[BrPred] Counter:\t%d, BrInst:\t%b, PredMade:\t%b, PredictedAddr:\t0x%x, MissPredict:\t%b, IncorrectAddr: %b\n",
    cntr,
    (dut.io.br_type =/= BR_XXX),
    dut.io.pred_made,
    dut.io.pred_addr,
    dut.io.pred_incorrect,
    dut.io.pred_wrong_addr
  )
}

class BrPredictorTests extends AnyFlatSpec with ChiselScalatestTester with Formal {

  "BrPredictorTestNoBranches" should "pass" in {
    test(new BrPredictorTestNoBranches(new BrPredictorOneEntry(32))).runUntilStop()
  }

}
