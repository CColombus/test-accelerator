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
    val IDLE, LOAD_REQ, LOAD_RESP, CALC, STORE_REQ, STORE_RESP = Value
  }

  import FSMstate._
  val state = RegInit(IDLE)

  val loadVect  = Reg(Vec(4, UInt(32.W)))
  val loadIdx   = RegInit(0.U(log2Ceil(4).W)) // index for the load vector
  val loadValid = RegInit(VecInit(Seq.fill(4)(false.B)))

  val cmd   = Queue(io.cmd)
  val funct = cmd.bits.inst.funct

  // define functions for the accelerator
  // val doStart = (funct === 0.U)
  val doAcc = funct === 1.U // load from register file

  // datapath
  val loadPtr  = cmd.bits.rs1 // pointer to the store address in memory
  val storePtr = cmd.bits.rs2 // pointer to the load address in memory

  // variables used to latch memory requests
  val reqAddr = Reg(UInt(32.W))
  val reqTag  = RegInit(0.U(log2Ceil(4).W)) // assuming 4 tags for the vector
  val reqData = Reg(UInt(32.W))
  val reqCmd  = RegInit(M_XRD)              // default to read command

  val memReqValid  = RegInit(false.B)
  val memRespValid = RegInit(true.B)

  // Function control signals
  val accDone = RegInit(false.B) // indicates if the accumulation is done

  // FSM
  switch(state) {
    is(IDLE) {
      when(cmd.valid && doAcc && !memReqValid) {
        printf(cf"*ta*IDLE~DEBUG~Received command, rs1: ${cmd.bits.rs1}, rs2: ${cmd.bits.rs2}\n")

        reqAddr     := loadPtr // + (loadIdx << 2.U) // assuming 4-byte words
        reqTag      := 0.U    // use the current index as the tag
        reqData     := 0.U        // no data to send yet
        reqCmd      := M_XRD      // set the command to read
        
        memReqValid := true.B     // indicate that a request is ready
        memRespValid := false.B    // response is not yet valid

        printf(cf"*ta*IDLE~Transition to LOAD_REQ state.\n")
        accDone := false.B // reset the done state
        state   := LOAD_REQ
      }
    }
    is(LOAD_REQ) {
      when(io.mem.req.fire) {
        memReqValid := false.B // request is sent, clear the valid state
        
        printf(cf"*ta*LOAD_REQ~Memory request sent for tag $reqTag at address $reqAddr with command $reqCmd.\n")
        
        printf(cf"*ta*LOAD_REQ~Transition to LOAD_RESP.\n")
        state       := LOAD_RESP
      }
    }
    is(LOAD_RESP) {
      when(io.mem.resp.valid) {

        memRespValid := true.B // response is valid now
        printf(cf"*ta*LOAD_RESP~Memory response received for tag ${io.mem.resp.bits.tag}.\n")

        printf(cf"*ta*LOAD_RESP~Loaded value: ${io.mem.resp.bits.data} into loadVect(${io.mem.resp.bits.tag}).\n")
        loadVect(io.mem.resp.bits.tag) := io.mem.resp.bits.data
        // loadValid(io.mem.resp.bits.tag) := true.B
        
        printf(cf"*ta*LOAD_RESP~Transition to CALC state.\n")
        state := CALC // transition to calculation state
      }
    }
    is(CALC) {
      // printf(cf"*ta*CALC~Performing calculation.\n")
      printf(cf"*ta*CALC~ Value loaded: ${loadVect(loadIdx)}.\n")
      printf(cf"*ta*CALC~Transition to STORE_REQ.\n")
      state := STORE_REQ
    }
    is(STORE_REQ) {
      when(io.mem.req.fire) {
        printf(cf"*ta*STORE_REQ~Memory request sent. Transition to STORE_RESP.\n")
        // memReqValid := false.B // request is sent, clear the valid state
        state       := STORE_RESP
      }.otherwise {
        // Latch the store request
        // reqAddr := storePtr
        // reqTag  := 0.U                     // tag does not matter for store
        // reqData := loadVect(loadIdx) + 3.U // example operation: add 3 to the loaded value
        // memReqValid := true.B // indicate that a request is ready
        // memRespValid := false.B // response is not yet valid
        printf(cf"*ta*STORE_REQ~Latched memory store request for tag $reqTag at address $reqAddr with data $reqData.\n")
      }
    }
    is(STORE_RESP) {
      when(io.mem.resp.valid) {
        printf(cf"*ta*STORE_RESP~Memory response received. Transition to IDLE.\n")
        accDone := true.B // mark the accumulation as done
        state   := IDLE
      }
    }
  }

  // COMMAND INTERFACE
  cmd.ready := !memReqValid // command is ready when in IDLE state

  // RESPONSE INTERFACE
  io.resp.valid   := cmd.valid && !memReqValid && memRespValid
  io.resp.bits.rd := cmd.bits.inst.rd // response register

// TODO: Change from here onwards
  // busy.reduce(_||_) --> OR of all busy vector registers
  io.busy := memReqValid || !memRespValid
  // Be busy when have pending memory requests or committed possibility of pending requests
  io.interrupt := false.B
  // Set this true to trigger an interrupt on the processor (please refer to supervisor documentation)

  // MEMORY REQUEST INTERFACE
  io.mem.req.valid        := memReqValid    // && !busy
  io.mem.req.bits.addr    := reqAddr
  io.mem.req.bits.tag     := reqTag
  io.mem.req.bits.cmd     := reqCmd
  io.mem.req.bits.size    := log2Ceil(64).U // 32 -> max 65536
  io.mem.req.bits.signed  := false.B
  io.mem.req.bits.data    := reqData
  io.mem.req.bits.phys    := false.B
  io.mem.req.bits.dprv    := cmd.bits.status.dprv
  io.mem.req.bits.dv      := cmd.bits.status.dv
  io.mem.req.bits.no_resp := false.B
}
