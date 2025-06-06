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
    val IDLE, STORE_REQ_SETUP, STORE_REQ_WAIT, STORE_REQ_FIRE, STORE_REQ_RESP, INST_COMPLETE = Value
  }

  import FSMstate._

  val regfile = Reg(Vec(4, UInt(32.W)))
  // val busy = RegInit(VecInit(Seq.fill(4){false.B})) // four busy registers for 4 registers

  val cmdQueue = Queue(io.cmd)
  val funct    = cmdQueue.bits.inst.funct

  // define functions for the accelerator
  // val doStart = (funct === 0.U)
  val doAccum = funct === 2.U // load from register file

  // datapath
  val rgPtr      = cmdQueue.bits.rs1                      // pointer to the location in memory
  val rgIdx      = cmdQueue.bits.rs2(log2Up(4) - 1, 0)    // idx of the register to write to
  val memRespTag = io.mem.resp.bits.tag(log2Up(4) - 1, 0) // tag for the memory response

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
        // rgRd  := cmdQueue.bits.inst.rd  // latch this because we dequeue it in fsm
        state := STORE_REQ_SETUP // move to setup state
      }
    }
    is(STORE_REQ_SETUP) {
      // latch request parameters
      printf(cf"*ta*Latching memory write request for $rgIdx at address $rgPtr with data $testReg.\n")
      reqAddr := rgPtr
      reqTag  := rgIdx
      reqData := testReg
      state   := STORE_REQ_FIRE // move to fire state
    }
    is(STORE_REQ_FIRE) {
      when(io.mem.req.fire) {
        printf(cf"*ta*Firing memory request for register $reqTag at address $reqAddr with data $reqData.\n")
        state := STORE_REQ_RESP // move to response state
      }
    }
    is(STORE_REQ_RESP) {
      when(io.mem.resp.valid) {
        printf(cf"*ta*Memory response received for tag $memRespTag.\n")
        state := INST_COMPLETE // move to calculation state
      }
    }
    is(INST_COMPLETE) {
      // perform any calculations for next store request
      testReg := testReg + 3.U // increment the test register
      printf(cf"*ta*Performing calculation, incremented test register to $testReg.\n")
      state := IDLE // go back to idle state
    }
  }

  when(io.mem.req.fire) {
    printf(cf"*ta*MONITOR~Memory request sent for register $reqTag at address $reqAddr with data $reqData.\n")
  }

// Memory request interface
  io.mem.req.valid        := (state === STORE_REQ_FIRE) // memReqValid // && !busy
  io.mem.req.bits.addr    := reqAddr
  io.mem.req.bits.tag     := reqTag
  io.mem.req.bits.cmd     := M_XWR
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
