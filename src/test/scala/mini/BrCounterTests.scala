package mini

import chisel3._
import chisel3.testers._
import chisel3.util._
import chiseltest._
import chiseltest.formal._
import org.scalatest.flatspec.AnyFlatSpec

class BrCounterOneEntryTest extends BasicTester with TestUtils {
  def xlen: Int = 32
  def address: Int = 100

  val dut = Module(new BranchCounterOneEntry(xlen))
  val ctrl = Module(new Control)

  // a NOP followed by multiple BEQs -- expected: do nothing (0), take (1), don't take (0), don't take (-1)
  override val insts = Seq(
    nop,
    B(Funct3.BEQ, 0, 0, 0),
    B(Funct3.BEQ, 1, 0, 0),
    B(Funct3.BEQ, 0, 1, 0)
  )

  val (cntr, done) = Counter(true.B, insts.size)

  // ===== OUTPUT CHECK HERE =====
//  val expected_cached_pc = VecInit((Seq(0) concat Seq.fill(insts.size - 1)(address)).map(_.U(xlen)))
//  val expected_times_taken = VecInit(Seq(0, 1, 0, -1).map(_.S))
  // ===== OUTPUT STOP  HERE =====

  // Connect Control
  ctrl.io.inst := VecInit(insts)(cntr)

  // Connect BrCounter
  dut.io.br_pc := address.U  // Always have a PC address of `address` for the branches
  dut.io.br_type := ctrl.io.br_type
  val branchTaken = VecInit(Seq(false, true, false, false).map(_.B))
  dut.io.br_taken := branchTaken(cntr)

  when (done) { stop() }

  printf(
    "[BrCounter Test] Counter: %d, BrType: %d, BrTaken: %d\n",
    cntr,
    dut.io.br_type,
    dut.io.br_taken
  )

}

class BrCounterTests extends AnyFlatSpec with ChiselScalatestTester with Formal {

  "BrCounterOneEntryTest" should "pass" in {
    test(new BrCounterOneEntryTest).runUntilStop()
  }

}