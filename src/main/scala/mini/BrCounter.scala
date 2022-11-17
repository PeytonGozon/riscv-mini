package mini

import chisel3.{Bool, Bundle, Input, Module, Output, UInt, chiselTypeOf, fromIntToWidth, when}

class FetchExecutePipelineRegisterWithBrCounter(xlen: Int) extends FetchExecutePipelineRegister(xlen) {
  val brCount = UInt(xlen.W)
}

//class BrCounterIO extends Bundle {
//  val taken = Input(UInt(4.W)) // Count up to 2^4 - 1 = 15 times
//  val count = Output(Int())
//}

//trait BranchCounterT {
//  def count(brCond: BrCond): Unit
//}
//
//class BrCountTotal extends BranchCounterT {
//  override def count(brCond: BrCond): Unit = {
//    when(brCond.io.taken) {
//      brCond.io.count += 1.U;
//    }
//  }
//}