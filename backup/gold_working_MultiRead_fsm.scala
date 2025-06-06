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
    val IDLE, READ_REQ_SETUP, READ_REQ_WAIT, READ_REQ_FIRE, READ_REQ_RESP, INST_COMPLETE = Value
  }

  import FSMstate._

  val regFile  = Reg(Vec(10, UInt(32.W)))
  val regValid = RegInit(VecInit(Seq.fill(10)(false.B))) // four busy registers for 4 registers
  val regIdx   = RegInit(0.U(4.W))                       // To keep track of which register we are working with

  val cmdQueue = Queue(io.cmd)
  val funct    = cmdQueue.bits.inst.funct

  // define functions for the accelerator
  // val doStart = (funct === 0.U)
  val doAccum = funct === 2.U // load from register file

  // datapath
  val rgPtr = cmdQueue.bits.rs1 // pointer to the location in memory

  val memRespTag = io.mem.resp.bits.tag(3, 0) // tag for the memory response

  val reqAddr = Reg(UInt(32.W))
  val reqTag  = Reg(UInt(4.W)) // or whatever width you need
  val reqData = Reg(UInt(32.W))

  val testReg = RegInit(318.asUInt(30.W))

  val state = RegInit(IDLE) // FSM state register

  // FSM logic
  switch(state) {
    is(IDLE) {
      when(cmdQueue.valid && doAccum) {
        printf(cf"*ta*RoCC cmd recieved.Transition STORE_REQ_SETUP state.\n")
        regIdx := 0.U // reset register index
        regValid.foreach(_ := false.B) // reset register valid flags
        state := READ_REQ_SETUP // move to setup state
      }
    }
    is(READ_REQ_SETUP) {
      // we go thorugh the register file as using the regIdx
      when(regValid.reduce(_ && _)) {
        // all registers have valid data,
        // TODO: handle this case, CALC maybe?
        state := INST_COMPLETE

      }.otherwise {
        // latch request parameters
        printf(cf"*ta*Latching memory write request for idx $regIdx.\n")
        // we offset the memory address by the register index
        reqAddr := rgPtr + (regIdx << 2).asUInt // assuming 4-byte registers
        reqTag  := regIdx
        reqData := 0.U                          // do not care
        state   := READ_REQ_FIRE               // move to fire state

      }

    }
    is(READ_REQ_FIRE) {
      when(io.mem.req.fire) {
        printf(cf"*ta*Firing memory request for register $reqTag at address $reqAddr.\n")
        state := READ_REQ_RESP // move to response state
      }
    }
    is(READ_REQ_RESP) {
      when(io.mem.resp.valid) {
        printf(cf"*ta*Memory response received for tag $memRespTag.\n")
        regValid(memRespTag) := true.B // mark the register as valid
        regFile(memRespTag) := io.mem.resp.bits.data // store the response data in the register file
        regIdx := regIdx + 1.U // increment the register index
        state := READ_REQ_SETUP // go back to setup state for next request
      }
    }
    is(INST_COMPLETE) {
      // perform any calculations for next store request
      // testReg := testReg + 3.U // increment the test register
      // printf(cf"*ta*Performing calculation, incremented test register to $testReg.\n")
      printf(cf"*ta*Instruction complete, all registers processed.\n")
      printf(cf"*ta*REGFILE: ${regFile}\n")
      state := IDLE // go back to idle state
    }
  }

  when(io.mem.req.fire) {
    // printf(cf"*ta*MONITOR~Memory request sent for register $reqTag at address $reqAddr with data $reqData.\n")
  }

// Memory request interface
  io.mem.req.valid        := (state === READ_REQ_FIRE) // memReqValid // && !busy
  io.mem.req.bits.addr    := reqAddr
  io.mem.req.bits.tag     := reqTag
  io.mem.req.bits.cmd     := M_XRD
  io.mem.req.bits.size    := log2Ceil(4).U              // 32 -> max 65536
  io.mem.req.bits.signed  := false.B
  io.mem.req.bits.data    := reqData
  io.mem.req.bits.phys    := false.B
  io.mem.req.bits.dprv    := cmdQueue.bits.status.dprv
  io.mem.req.bits.dv      := cmdQueue.bits.status.dv
  io.mem.req.bits.no_resp := false.B

  io.resp.bits.rd := cmdQueue.bits.inst.rd // response register

// ready means we dequeue the command buffer
  cmdQueue.ready := (state === INST_COMPLETE)
  io.resp.valid  := (state === INST_COMPLETE)

  io.busy      := (state =/= IDLE) // busy when not in IDLE state
  io.interrupt := false.B
}
