package testaccelerator

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tile._
import chisel3.util.switch
import chisel3.util.is

class TestAccelerator(opcodes: OpcodeSet, val n: Int = 4)(implicit p: Parameters) extends LazyRoCC(opcodes) {
  override lazy val module = new TestAcceleratorModule(this)
}

class TestAcceleratorModule(outer: TestAccelerator)(implicit p: Parameters)
    extends LazyRoCCModuleImp(outer)
    with HasCoreParameters {

  // Define FSM states
  object FSMstate extends ChiselEnum {
    val IDLE,
      QSP_READ_REQ_SETUP, QSP_READ_REQ_FIRE, QSP_READ_REQ_RESP, QSP_RET_QSPAN,
      LPA_READ_REQ_SETUP, LPA_READ_REQ_FIRE, LPA_READ_REQ_RESP,
      INST_COMPLETE = Value
  }

  import FSMstate._

  val cmdQueue = Queue(io.cmd)
  val funct    = cmdQueue.bits.inst.funct

  // define functions for the accelerator
  val doQspan      = funct === 0.U
  val doLoadParams = funct === 1.U

  // datapath
  val cmdRs1   = cmdQueue.bits.rs1
  val cmdRs2   = cmdQueue.bits.rs2
  val respData = RegInit(0.U(32.W)) // response data to be sent back to the processor

  val memRespTag = io.mem.resp.bits.tag(3, 0) // tag for the memory response

  val reqAddr = Reg(UInt(32.W))
  val reqTag  = Reg(UInt(4.W))         // or whatever width you need
  val reqData = Reg(UInt(32.W))
  val reqCmd  = RegInit(M_XRD)         // default command is read
  val reqSize = RegInit(log2Ceil(4).U) // default size is 4 bytes

  val state = RegInit(IDLE) // FSM state register

  // sumQspan logic
  val addrOfFirstY = Reg(UInt(32.W))    // pointer to the location in memory
  val sumQspan     = RegInit(0.U(32.W)) // accumulator for Qspan
  val nCounter     = RegInit(0.U(32.W)) // counter for n anchors
  val nMax         = RegInit(0.U(32.W)) // counter for n anchors
  val truncY       = Wire(UInt(8.W))    // truncated y value for Qspan
  truncY := (io.mem.resp.bits.data >> 32)(7, 0)

  // Common parameters
  val addrOfAnchorArray = RegInit(0.U(32.W)) // base address of the anchor array

  // Param loading logic
  val addrOfParamsArray = RegInit(0.U(32.W))                        // base address of the parameter array
  val regParams         = RegInit(VecInit(Seq.fill(20)(0.U(64.W)))) // register file for parameters
  val regFillCounter    = RegInit(0.U(5.W))                         // counter for filling parameters

  // FSM logic
  switch(state) {
    is(IDLE) {
      when(cmdQueue.valid && doQspan) {
        printf(cf"*ta*QSPAN start.Transition STORE_REQ_SETUP state.\n")
        nMax              := cmdRs2                   // nMax is the number of anchors
        nCounter          := 0.U                      // reset the counter
        sumQspan          := 0.U                      // reset the Qspan accumulator
        addrOfAnchorArray := cmdRs1                   // base address of the anchor array, useful later
        addrOfFirstY      := cmdRs1 + (1 << 3).asUInt // assuming 8-byte integers and a[0].y is at offset 8
        state             := QSP_READ_REQ_SETUP       // move to setup state
      }
        .elsewhen(cmdQueue.valid && doLoadParams) {
          printf(cf"*ta*LPARAMS start.Transition STORE_REQ_SETUP state.\n")
          regFillCounter := 0.U // reset the parameter fill counter
          addrOfParamsArray := cmdRs1 // base address of the parameter array
          state := LPA_READ_REQ_SETUP // move to parameter loading setup state
        }
    }
    is(QSP_READ_REQ_SETUP) {
      // we sum up all the a[i].y upto n
      when(nCounter === nMax) {
        // we hav completed the Qspan operation
        state := QSP_RET_QSPAN

      }.otherwise {
        // latch request parameters
        printf(cf"*ta*LMR request for idx $nCounter.\n")
        // we offset the memory address by the every other (jump 16) register index
        reqAddr := addrOfFirstY + (nCounter << 4).asUInt // assuming two 8-byte integers in each element as {a[i].x, a[i].y}
        reqTag  := nCounter(3, 0)                        // use nCounter as tag for the memory request
        reqData := 0.U                                   // do not care
        reqCmd  := M_XRD                                 // read command
        reqSize := log2Ceil(8).U                         // size is 8 bytes (for 64-bit integers)
        state   := QSP_READ_REQ_FIRE                     // move to fire state

      }

    }
    is(QSP_READ_REQ_FIRE) {
      when(io.mem.req.fire) {
        printf(cf"*ta*Firing memory request for idx $reqTag at address $reqAddr.\n")
        state := QSP_READ_REQ_RESP // move to response state
      }
    }
    is(QSP_READ_REQ_RESP) {
      when(io.mem.resp.valid) {
        // printf(cf"*ta*Memory response received for tag $memRespTag.\n")
        sumQspan := sumQspan + truncY // accumulate the y value
        printf(cf"*ta*Accumulated Qspan value: $sumQspan. from resp tag $memRespTag.\n")
        nCounter := nCounter + 1.U     // increment the counter
        state    := QSP_READ_REQ_SETUP // go back to setup state for next request
      }
    }
    is(QSP_RET_QSPAN) {
      // return the Qspan result
      printf(cf"*ta*Returning Qspan result: $sumQspan.\n")
      respData := sumQspan      // set the response data
      state    := INST_COMPLETE // move to instruction complete state
    }

    is(LPA_READ_REQ_SETUP) {
      // we load parameters into the register file
      when(regFillCounter === 16.U) {
        // we have filled all the registers
        printf(cf"*ta*Loaded all parameters into registers.\n")
        printf(cf"*ta*Register parameters: $regParams.\n")
        state := INST_COMPLETE

      }.otherwise {
        // latch request parameters
        printf(cf"*ta*LPA request for idx $regFillCounter.\n")
        // we offset the memory address by the register index
        reqAddr := addrOfParamsArray + (regFillCounter << 3).asUInt // assuming 8-byte integers
        reqTag  := regFillCounter(3, 0)                             // use regFillCounter as tag for the memory request
        reqData := 0.U                                              // do not care
        reqCmd  := M_XRD                                            // read command
        reqSize := log2Ceil(8).U                                    // size is 8 bytes (for 64-bit integers)
        state   := LPA_READ_REQ_FIRE                                // move to fire state

      }
    }
    is(LPA_READ_REQ_FIRE) {
      when(io.mem.req.fire) {
        printf(cf"*ta*LPA Firing memory request for idx $reqTag at address $reqAddr.\n")
        state := LPA_READ_REQ_RESP // move to response state
      }
    }
    is(LPA_READ_REQ_RESP) {
      when(io.mem.resp.valid) {
        // printf(cf"*ta*LPA Memory response received for tag $memRespTag.\n")
        regParams(regFillCounter) := io.mem.resp.bits.data // store the response data in the register file
        printf(cf"*ta*LPA Loaded parameter: ${regParams(regFillCounter)} into register $regFillCounter.\n")
        regFillCounter := regFillCounter + 1.U // increment the fill counter
        state          := LPA_READ_REQ_SETUP   // go back to setup state for next request
      }
    }
    
    
    
    is(INST_COMPLETE) {
      printf(cf"*ta*Instruction complete.\n")
      state := IDLE // go back to idle state
    }
  }

// Memory request interface
  io.mem.req.valid        := (state === QSP_READ_REQ_FIRE || state === LPA_READ_REQ_FIRE)
  io.mem.req.bits.addr    := reqAddr
  io.mem.req.bits.tag     := reqTag
  io.mem.req.bits.cmd     := reqCmd
  io.mem.req.bits.size    := reqSize
  io.mem.req.bits.signed  := true.B
  io.mem.req.bits.data    := reqData
  io.mem.req.bits.phys    := false.B
  io.mem.req.bits.dprv    := cmdQueue.bits.status.dprv
  io.mem.req.bits.dv      := cmdQueue.bits.status.dv
  io.mem.req.bits.no_resp := false.B

  io.resp.bits.rd := cmdQueue.bits.inst.rd // response register

// ready means we dequeue the command buffer
  cmdQueue.ready := (state === INST_COMPLETE)
  // response interface
  io.resp.bits.data := respData // send the response data if available
  io.resp.valid     := (state === INST_COMPLETE)

  io.busy      := (state =/= IDLE) // busy when not in IDLE state
  io.interrupt := false.B
}
