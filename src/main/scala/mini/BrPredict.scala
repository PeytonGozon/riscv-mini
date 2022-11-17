package mini

import chisel3._
import mini.Control._


//class BrPredictIO(xlen: Int) extends Bundle {
class BrPredictIO extends Bundle {
  val prev_branch: Bool = Input(Bool())
  val prediction: Bool = Output(Bool())
}

trait BrPredict extends Module {
//  def xlen: Int
  val io: BrPredictIO
}

class BrPredictStaticTake extends BrPredict {
  val io: BrPredictIO = IO(new BrPredictIO())
  io.prediction := true.B // statically always output "true" / "take"
}

class BrPredict1BitDynamic extends BrPredict {
  val io: BrPredictIO = IO(new BrPredictIO())
  io.prediction := io.prev_branch // map the previous branch to the output.
}
