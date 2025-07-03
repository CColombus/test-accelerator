package testaccelerator

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tile._
import chisel3.util.switch
import chisel3.util.is

// Ilog32 implementation in Chisel
class ILOG extends Module {
  val io = IO(new Bundle {
    val in  = Input(SInt(32.W))
    val out = Output(SInt(8.W))
  })

  val x        = io.in
  val res      = Wire(SInt(8.W))
  val zeroCase = x === 0.S

  val r16 = Mux(x >= (1.S << 16), 16.S, 0.S)
  val x1  = Mux(x >= (1.S << 16), (x >> 16).asSInt, x)

  val r8 = Mux(x1 >= (1.S << 8), 8.S, 0.S)
  val x2 = Mux(x1 >= (1.S << 8), (x1 >> 8).asSInt, x1)

  val r4 = Mux(x2 >= (1.S << 4), 4.S, 0.S)
  val x3 = Mux(x2 >= (1.S << 4), (x2 >> 4).asSInt, x2)

  val r2 = Mux(x3 >= (1.S << 2), 2.S, 0.S)
  val x4 = Mux(x3 >= (1.S << 2), (x3 >> 2).asSInt, x3)

  val r1 = Mux(x4 >= (1.S << 1), 1.S, 0.S)

  res := r16 + r8 + r4 + r2 + r1

  io.out := Mux(zeroCase, -1.S, res)
}

class FixMult64 extends Module {
  val io = IO(new Bundle {
    val ina = Input(SInt(64.W))
    val inb = Input(SInt(64.W))
    val out = Output(SInt(64.W))
  })

  val mult_temp = Wire(SInt(128.W))
  mult_temp := io.ina * io.inb

  io.out := (mult_temp >> 32).asSInt
}

class TestAccelerator(opcodes: OpcodeSet, val n: Int = 4)(implicit p: Parameters) extends LazyRoCC(opcodes) {
  override lazy val module = new TestAcceleratorModule(this)
}

class TestAcceleratorModule(outer: TestAccelerator)(implicit p: Parameters)
    extends LazyRoCCModuleImp(outer)
    with HasCoreParameters {

  // Define FSM states
  object FSMstate extends ChiselEnum {
    val IDLE, QSP_READ_REQ_SETUP, QSP_READ_REQ_FIRE, QSP_READ_REQ_RESP, QSP_RET_QSPAN, LPA_READ_REQ_SETUP,
        LPA_READ_REQ_FIRE, LPA_READ_REQ_RESP, COJ_READ_REQ_SETUP, COJ_READ_REQ_FIRE, COJ_READ_REQ_RESP, COJ_CALCULATION,
        INST_COMPLETE = Value
  }

  import FSMstate._

  val cmdQueue = Queue(io.cmd)
  val funct    = cmdQueue.bits.inst.funct

  // define functions for the accelerator
  val doQspan      = funct === 0.U
  val doLoadParams = funct === 1.U
  val doCalOneJ    = funct === 2.U

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
  val addrOfBaseY = Reg(UInt(32.W))    // pointer to the location in memory
  val sumQspan    = RegInit(0.U(32.W)) // accumulator for Qspan
  val nCounter    = RegInit(0.U(32.W)) // counter for n anchors
  val nMax        = RegInit(0.U(32.W)) // counter for n anchors
  val truncY      = Wire(UInt(8.W))    // truncated y value for Qspan
  truncY := (io.mem.resp.bits.data >> 32)(7, 0)

  // Common parameters
  val addrOfBaseX = RegInit(0.U(32.W)) // base address of the anchor array

  // Param loading logic
  val constParamCount   = 7
  val addrOfParamsArray = RegInit(0.U(32.W))                    // base address of the parameter array
  val regParams         = Reg(Vec(constParamCount, SInt(64.W))) // register array for parameters
  val regFillCounter    = RegInit(0.U(5.W))                     // counter for filling parameters

  val P_is_cdna   = regParams(0)
  val p_ri        = regParams(1)
  val p_qi        = regParams(2)
  val p_qspan     = regParams(3)
  val p_sidi      = regParams(4)
  val p_avg_qspan = regParams(5)
  val p_gap_scale = regParams(6)

  // Calculate one J logic
  val idx_j         = RegInit(0.U(32.W))  // index for J
  val offset_j      = RegInit(0.U(5.W))   // counter for filling parameters
  val regAJset      = Reg(Vec(2, SInt(64.W)))
  val regValidAJset = Reg(Vec(2, Bool())) // validity flags for AJset elements

  val v_a_j_x = regAJset(0) // x coordinate of AJset
  val v_a_j_y = regAJset(1) // y coordinate of AJset

  val v_dr = Wire(SInt(64.W))
  v_dr := p_ri - v_a_j_x
  val v_dq = Wire(SInt(64.W))
  v_dq := p_qi - v_a_j_y(31, 0).asSInt
  val v_sidj = Wire(SInt(8.W))
  v_sidj := v_a_j_y(55, 48).asSInt

  val v_dd     = Mux(v_dr > v_dq, v_dr - v_dq, v_dq - v_dr) // absolute difference between V_dr and V_dq
  val v_min_dq = Mux(v_dq < v_dr, v_dq, v_dr)               // minimum of V_dq and V_dr
  val v_sc_1   = Mux(v_min_dq > p_qspan, p_qspan, v_min_dq)

  val f_ilog32 = Module(new ILOG) // instantiate the ILOG module
  f_ilog32.io.in := v_dd(31, 0).asSInt // input to the ILOG module is the lower 32 bits of V_dd
  val v_log_dd = Mux(v_dd > 0.S, f_ilog32.io.out, 0.S)

  val scale       = BigInt(1) << 32
  val scaleSInt   = scale.S(64.W)

  val v_gap_cost  = Wire(SInt(64.W))
  val f_mult64_u1 = Module(new FixMult64) // instantiate the fixed-point multiplier
  val f_mult64_u2 = Module(new FixMult64) // instantiate the fixed-point multiplier

  f_mult64_u1.io.ina := v_dd * scaleSInt
  f_mult64_u1.io.inb := (0.01 * scale.toDouble).toLong.S

  f_mult64_u2.io.ina := f_mult64_u1.io.out
  f_mult64_u2.io.inb := p_avg_qspan

  v_gap_cost := f_mult64_u2.io.out + ((v_log_dd >> 1) * scaleSInt)

  // FSM logic
  switch(state) {
    is(IDLE) {
      when(cmdQueue.valid && doQspan) {
        printf(cf"*ta*QSPAN start.\n")
        nMax        := cmdRs2                   // nMax is the number of anchors
        nCounter    := 0.U                      // reset the counter
        sumQspan    := 0.U                      // reset the Qspan accumulator
        addrOfBaseX := cmdRs1                   // base address of the anchor array, a[0].x is at offset 0
        addrOfBaseY := cmdRs1 + (1 << 3).asUInt // assuming 8-byte integers and a[0].y is at offset 8
        state       := QSP_READ_REQ_SETUP       // move to setup state
      }
        .elsewhen(cmdQueue.valid && doLoadParams) {
          printf(cf"*ta*LPARAMS start.\n")
          regFillCounter    := 0.U                // reset the parameter fill counter
          addrOfParamsArray := cmdRs1             // base address of the parameter array
          state             := LPA_READ_REQ_SETUP // move to parameter loading setup state
        }
        .elsewhen(cmdQueue.valid && doCalOneJ) {
          printf(cf"*ta*CALONEJ start.\n")
          regValidAJset.foreach(_ := false.B) // reset validity flags
          idx_j    := cmdRs1
          offset_j := 0.U // reset offset for J
          state    := COJ_READ_REQ_SETUP
        }
    }
    is(QSP_READ_REQ_SETUP) {
      // we sum up all the a[i].y upto n
      when(nCounter === nMax) {
        // we hav completed the Qspan operation
        state := QSP_RET_QSPAN

      }.otherwise {
        // latch request parameters
        // printf(cf"*ta*LMR request for idx $nCounter.\n")
        // we offset the memory address by the every other (jump 16) register index
        reqAddr := addrOfBaseY + (nCounter << 4).asUInt // assuming two 8-byte integers in each element as {a[i].x, a[i].y}
        reqTag  := nCounter(3, 0)                       // use nCounter as tag for the memory request
        reqData := 0.U                                  // do not care
        reqCmd  := M_XRD                                // read command
        reqSize := log2Ceil(8).U                        // size is 8 bytes (for 64-bit integers)
        state   := QSP_READ_REQ_FIRE                    // move to fire state

      }

    }
    is(QSP_READ_REQ_FIRE) {
      when(io.mem.req.fire) {
        // printf(cf"*ta*Firing memory request for idx $reqTag at address $reqAddr.\n")
        state := QSP_READ_REQ_RESP // move to response state
      }
    }
    is(QSP_READ_REQ_RESP) {
      when(io.mem.resp.valid) {
        // printf(cf"*ta*Memory response received for tag $memRespTag.\n")
        sumQspan := sumQspan + truncY // accumulate the y value
        // printf(cf"*ta*Accumulated Qspan value: $sumQspan. from resp tag $memRespTag.\n")
        nCounter := nCounter + 1.U     // increment the counter
        state    := QSP_READ_REQ_SETUP // go back to setup state for next request
      }
    }
    is(QSP_RET_QSPAN) {
      // return the Qspan result
      printf(cf"*ta*Returning Sum Qspan result: $sumQspan.\n")
      respData := sumQspan      // set the response data
      state    := INST_COMPLETE // move to instruction complete state
    }

    is(LPA_READ_REQ_SETUP) {
      // we load parameters into the register file
      when(regFillCounter === constParamCount.asUInt) {
        // we have filled all the registers
        printf(cf"*ta*Loaded all parameters into registers.\n")
        printf(cf"*ta*Register parameters: $regParams.\n")
        // print p_avg_qspan and p_gap_scale
        printf(cf"*ta*p_avg_qspan: ${p_avg_qspan}, p_gap_scale: ${p_gap_scale}.\n")
        // set the response data to the first parameter
        state := INST_COMPLETE

      }.otherwise {
        // latch request parameters
        // printf(cf"*ta*LPA request for idx $regFillCounter.\n")
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
        // printf(cf"*ta*LPA Firing memory request for idx $reqTag at address $reqAddr.\n")
        state := LPA_READ_REQ_RESP // move to response state
      }
    }
    is(LPA_READ_REQ_RESP) {
      when(io.mem.resp.valid) {
        // printf(cf"*ta*LPA Memory response received for tag $memRespTag.\n")
        regParams(regFillCounter) := io.mem.resp.bits.data.asSInt // store the response data in the register file
        // printf(cf"*ta*LPA Loaded parameter: ${regParams(regFillCounter)} into register $regFillCounter.\n")
        regFillCounter := regFillCounter + 1.U // increment the fill counter
        state          := LPA_READ_REQ_SETUP   // go back to setup state for next request
      }
    }

    is(COJ_READ_REQ_SETUP) {
      when(regValidAJset.reduce(_ && _)) {
        // all AJset elements are valid, we can proceed to calculate
        printf(cf"*ta*All AJset elements are valid, proceeding to calculation.\n")
        state := COJ_CALCULATION
      }.otherwise {
        // printf(cf"*ta*CALONEJ request setup for idx $idx_j.\n")
        // we offset the memory address by the register index
        // assuming two 8-byte integers in each element as {a[i].x, a[i].y}
        // and subsequent x and y elements are at offsets of 8 bytes
        reqAddr := addrOfBaseX + (idx_j << 4).asUInt + (offset_j << 3).asUInt // offset_j is used to access subsequent .x .y elements
        reqTag  := offset_j(3, 0)                                             // use offset_j as tag for the memory request
        reqData := 0.U                                                        // do not care
        reqCmd  := M_XRD                                                      // read command
        reqSize := log2Ceil(8).U                                              // size is 8 bytes (for 64-bit integers)
        state   := COJ_READ_REQ_FIRE                                          // move to fire state
      }
    }
    is(COJ_READ_REQ_FIRE) {
      when(io.mem.req.fire) {
        // printf(cf"*ta*CALONEJ Firing memory request for idx $reqTag at address $reqAddr.\n")
        state := COJ_READ_REQ_RESP // move to response state
      }
    }
    is(COJ_READ_REQ_RESP) {
      when(io.mem.resp.valid) {
        // printf(cf"*ta*CALONEJ Memory response received for tag $memRespTag.\n")
        regValidAJset(offset_j) := true.B                       // mark the AJset element as valid
        regAJset(offset_j)      := io.mem.resp.bits.data.asSInt // store the response data
        // printf(cf"*ta*CALONEJ Loaded AJset element: ${regValidAJset(offset_j)} at offset $offset_j.\n")
        offset_j := offset_j + 1.U     // increment the offset for the next .x .y element
        state    := COJ_READ_REQ_SETUP // go back to setup state for next request
      }
    }
    is(COJ_CALCULATION) {
      // perform the calculation for one J
      // printf(cf"*ta*Performing calculation for J with idx $idx_j.\n")
      printf(cf"*ta*a[j].x: $v_a_j_x, a[j].y: $v_a_j_y, dr: $v_dr, dq: $v_dq, sidj: $v_sidj.\n")
      printf(
        cf"*ta*dd: $v_dd, min_dq: $v_min_dq, sc_1: $v_sc_1, log_dd: $v_log_dd, gap_cost: $v_gap_cost.\n"
      )

      // Here you can add more calculations or logic as needed
      respData := v_gap_cost.asUInt // set the response data to the log value
      state    := INST_COMPLETE     // move to instruction complete state
    }

    is(INST_COMPLETE) {
      printf(cf"*ta*Instruction complete.\n")
      state := IDLE // go back to idle state
    }
  }

// Memory request interface
  io.mem.req.valid        := (state === QSP_READ_REQ_FIRE || state === LPA_READ_REQ_FIRE || state === COJ_READ_REQ_FIRE)
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
