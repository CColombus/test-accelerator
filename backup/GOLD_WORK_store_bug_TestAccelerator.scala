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
    val IDLE, READ_REQ_SETUP, READ_REQ_FIRE, READ_REQ_RESP, STORE_REQ_SETUP, STORE_REQ_FIRE, STORE_REQ_RESP,
        SYNC_SETUP, SYNC_FIRE, SYNC_RESP, INST_COMPLETE = Value
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
  val rgPtr   = cmdQueue.bits.rs1 // pointer to the location in memory
  val rgWrPtr = cmdQueue.bits.rs2 // pointer to the write location in memory

  val memRespTag = io.mem.resp.bits.tag(3, 0) // tag for the memory response

  val reqAddr = Reg(UInt(32.W))
  val reqTag  = Reg(UInt(4.W)) // or whatever width you need
  val reqData = Reg(UInt(32.W))
  val reqCmd  = RegInit(M_XRD) // default command is read
  val reqSize = RegInit(2.U(4.W)) // default size is 4 bytes

  val state = RegInit(IDLE) // FSM state register

  // accumulator logic
  val sum = Wire(UInt(32.W))
  sum := regFile.reduce(_ + _) // sum all valid registers

  // write again because we need to let the memory settle
  val wrCounter = RegInit(0.U(4.W))

  // FSM logic
  switch(state) {
    is(IDLE) {
      when(cmdQueue.valid && doAccum) {
        printf(cf"*ta*RoCC cmd recieved.Transition STORE_REQ_SETUP state.\n")
        regIdx    := 0.U // reset register index
        wrCounter := 0.U // reset write counter
        regValid.foreach(_ := false.B) // reset register valid flags
        state := READ_REQ_SETUP // move to setup state
      }
    }
    is(READ_REQ_SETUP) {
      // we go thorugh the register file as using the regIdx
      when(regValid.reduce(_ && _)) {
        // all registers have valid data,
        // TODO: handle this case, CALC maybe?
        state := STORE_REQ_SETUP

      }.otherwise {
        // latch request parameters
        printf(cf"*ta*Latching memory write request for idx $regIdx.\n")
        // we offset the memory address by the register index
        reqAddr := rgPtr + (regIdx << 2).asUInt // assuming 4-byte registers
        reqTag  := regIdx
        reqData := 0.U                          // do not care
        reqCmd  := M_XRD                        // read command
        reqSize := 2.U
        state   := READ_REQ_FIRE                // move to fire state

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
        regValid(memRespTag) := true.B                // mark the register as valid
        regFile(memRespTag)  := io.mem.resp.bits.data // store the response data in the register file
        regIdx               := regIdx + 1.U          // increment the register index
        state                := READ_REQ_SETUP        // go back to setup state for next request
      }
    }

    // READ REQUESTS are done, now we can process the STORE REQUESTS
    is(STORE_REQ_SETUP) {
        printf(cf"*ta*Latching memory write request for address $rgWrPtr with data $sum.\n")
        reqAddr := rgWrPtr
        reqTag  := 15.U
        reqData := sum
        reqCmd  := M_XWR          // write command
        reqSize := 2.U
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
        printf(cf"*ta*Memory write response received for tag $memRespTag.\n")
        state := SYNC_SETUP // go back to setup state for next request
      }
    }

    // Try reading back again to ensure the data is written
    is(SYNC_SETUP) {
      printf(cf"*ta*Latching memory SYNC request for address $rgWrPtr.\n")
      reqAddr := rgWrPtr
      reqTag  := 15.U
      reqData := 0.U // do not care
      reqCmd  := M_XRD // read command
      reqSize := 2.U
      state   := SYNC_FIRE // move to fire state
    }
    is(SYNC_FIRE) {
      when(io.mem.req.fire) {
        printf(cf"*ta*Firing memory read request for register $reqTag at address $reqAddr.\n")
        state := SYNC_RESP // move to response state
      }
    }
    is(SYNC_RESP) {
      when(io.mem.resp.valid) {
        printf(cf"*ta*Memory SYNC response received for tag $memRespTag with data ${io.mem.resp.bits.data}.\n")
        // Check if the data matches the expected sum
        when(io.mem.resp.bits.data === sum) {
          printf(cf"*ta*Data verification successful. Sum matches expected value.\n")
        }.otherwise {
          printf(cf"*ta*Data verification failed. Expected $sum but got ${io.mem.resp.bits.data}.\n")
        }
        state := INST_COMPLETE // move to instruction complete state
      }
    }

    is(INST_COMPLETE) {
      printf(cf"*ta*Instruction complete.\n")
      printf(cf"*ta*REGFILE: $regFile\n")
      state := IDLE // go back to idle state
    }
  }

  // when(io.mem.req.fire) {
  //   // printf(cf"*ta*MONITOR~Memory request sent for register $reqTag at address $reqAddr with data $reqData.\n")
  // }

// Memory request interface
  io.mem.req.valid        := (state === READ_REQ_FIRE) || (state === STORE_REQ_FIRE) || (state === SYNC_FIRE)
  io.mem.req.bits.addr    := reqAddr
  io.mem.req.bits.tag     := reqTag
  io.mem.req.bits.cmd     := reqCmd
  io.mem.req.bits.size    := reqSize
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
