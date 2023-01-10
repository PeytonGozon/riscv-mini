package mini

import chisel3._
import mini.Control._

class BranchCounterIO(xlen: Int) extends Bundle {

  // Inputs [EXECUTE]
  var br_type: UInt = Input(UInt(3.W))  // The same as in BrCond
  var br_taken: Bool = Input(Bool())
  var br_pc: UInt = Input(UInt(xlen.W))

  // Outputs
  // None, thus far.

}

trait BranchCounter extends Module {
  def xlen: Int
  val io: BranchCounterIO
}

class BranchCounterOneEntry(val xlen: Int) extends BranchCounter {
  val io: BranchCounterIO = IO(new BranchCounterIO(xlen))

  val cached_pc: UInt = RegInit(0.U(xlen.W))
  val times_taken: SInt = RegInit(0.S(xlen.W))

  // When the current instruction in the EXECUTE stage is a branch
  when (io.br_type =/= BR_XXX) {
    printf("[BrCounter] cached pc: 0x%x, times_taken: %d\n", cached_pc, times_taken)

    // If the PC is in the table or not.
    when (io.br_pc === cached_pc) {
      times_taken := times_taken + Mux(io.br_taken, 1.S(xlen.W), -1.S(xlen.W))
      printf("[BrCounter] branch: 0x%x, taken: %d, times taken: %d\n", io.br_pc, io.br_taken.asUInt, times_taken)
    } otherwise {
      cached_pc := io.br_pc
      times_taken := 0.S(xlen.W)
      printf("[BrCounter] new branch pc: 0x%x\n", io.br_pc)
    }

  } otherwise {
    printf("[BrCounter] Non-branch instruction\n")
  }
}

class BranchCounterTable(val xlen: Int) extends BranchCounter {
  val io: BranchCounterIO = IO(new BranchCounterIO(xlen))


}
