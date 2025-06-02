  object FSMstate extends ChiselEnum {
    val IDLE, LOAD, CALC, STORE, CLEANUP = Value
  }

  import FSMstate._

  val state = RegInit(IDLE)

  // define vector size
  val vectorSize = 5 // size of the vector to load
  val loadVect   = Reg(Vec(vectorSize, UInt(32.W)))
  val validVect = RegInit(VecInit(Seq.fill(vectorSize)(false.B)))
  val vectIdx   = RegInit(0.U(log2Up(vectorSize).W)) // index for the vector

  // Perform the accumulation
  val sum = loadVect.reduce(_ + _)

  // store done
  val storeDone = RegInit(false.B)

  val cmd   = Queue(io.cmd)
  val funct = cmd.bits.inst.funct
  // val addr = cmd.bits.rs2(log2Up(outer.n)-1,0)

  // define functions for the accelerator
  val doAccum = funct === 2.U // load from register file

  // datapath
  val rgPtr      = cmd.bits.rs1  // pointer to the location in memory
  val rgStorePtr = cmd.bits.rs2 // pointer to the location in memory for storing the result
  // val rgIdx      = cmd.bits.rs2(log2Up(vectorSize) - 1, 0)         // idx of the register to write to
  val memRespTag = io.mem.resp.bits.tag(log2Up(vectorSize) - 1, 0) // tag for the memory response

// Add a register to hold the request valid state
  val memReqValid  = RegInit(false.B)
  val memRespValid = RegInit(true.B)

  val reqAddr = Reg(UInt(32.W))
  val reqTag  = Reg(UInt(4.W)) // or whatever width you need
  val reqData = Reg(UInt(32.W))

  // FSM
  switch(state) {
    is(IDLE) {
      when(cmd.valid && doAccum) {
        state := LOAD

        // Initiate the first memory request
        vectIdx := 0.U // reset the vector index
        storeDone := false.B // reset the store done flag
        reqAddr := rgPtr // set the request address to the register pointer
        reqTag := 0.U // start with the first tag
        reqData := 0.U // no data to send yet
        memReqValid  := true.B // indicate that a memory request is ready
        memRespValid := false.B // response is not ready yet
        printf(cf"*ta*Transition to LOAD state.\n")
      }.otherwise {
        // If not accumulating, stay in IDLE state
        memReqValid  := false.B // clear the request valid state
      }
    }
    is(LOAD) {
      // TODO: For now check with only idx 0
      when(io.mem.resp.valid) {
        // Load the data from memory response into the vector
        loadVect(memRespTag)  := io.mem.resp.bits.data(31, 0)
        validVect(memRespTag) := true.B
        printf(cf"*ta*Loaded data ${io.mem.resp.bits.data(31, 0)} into vector at index $memRespTag.\n")
        state := CALC // transition to CALC state
        printf(cf"*ta*Transition to CALC state.\n")
        // val nextIdx = vectIdx + 1.U
        // when(nextIdx < vectorSize.U) {
        //   vectIdx := nextIdx // increment the index for the next load
        //   // Initiate next memory fetch
        //   reqAddr      := rgPtr + (nextIdx << 3) // increment address by 8 (word-aligned, 8 bytes)
        //   reqTag       := nextIdx
        //   memReqValid  := true.B
        //   memRespValid := false.B
        //   printf(cf"*ta*Latch memory fetch request for index $nextIdx.\n")
        // }.otherwise {
        //   printf(cf"*ta*All vector elements loaded.\n")
        // }
      }.elsewhen(io.mem.req.fire) {
        // Memory request sent, clear the valid state
        memReqValid := false.B
        printf(cf"*ta*LS~Memory request sent vector index $vectIdx address $reqAddr request tag $reqTag\n")
        // debug io.mem.req.bits  
        printf(cf"*ta*LS~Mem.req.bits: addr=${io.mem.req.bits.addr}, tag=${io.mem.req.bits.tag}, cmd=${io.mem.req.bits.cmd}, size=${io.mem.req.bits.size}, data=${io.mem.req.bits.data}\n")
      }
    }
    is(CALC) {
      printf(cf"*ta*Accumulated sum: $sum.\n")
      state := STORE
      printf(cf"*ta*Transition to STORE state.\n")
    }
    is(STORE) {
      when(io.mem.resp.valid && storeDone) {
        memReqValid  := false.B // clear the request valid state
        state := CLEANUP
        printf(cf"*ta*Transition to CLEANUP state.\n")
      }.otherwise {
        // Store the sum back to memory
        reqAddr := rgStorePtr
        // reqTag       := rgIdx
        reqData      := sum
        memReqValid  := true.B
        memRespValid := false.B
        storeDone    := true.B // indicate that the store is done
        printf(cf"*ta*Latched memory write request for sum $sum at index $rgStorePtr.\n")
      }
    }
    is(CLEANUP) {
      memRespValid := true.B  // indicate that the response is ready
      memReqValid  := false.B // clear the request valid state
      // Reset the vector and index for the next operation
      loadVect.foreach(_ := 0.U) // clear the vector
      validVect.foreach(_ := false.B) // clear the valid state
      state := IDLE
      printf(cf"*ta*Transition to IDLE state.\n")
    }
  }

  // // When a command comes in and we're not busy, latch the request
  // when(cmd.valid && doAccum && !memReqValid) {
  //   reqAddr      := rgPtr
  //   reqTag       := rgIdx
  //   reqData      := BigInt("4294967295").U(32.W)
  //   memReqValid  := true.B  // request ready to be sent
  //   memRespValid := false.B // reponse yet to come
  //   printf(cf"*ta*Latched memory write request.\n")
  // }

  // // When the request is accepted by memory, clear reqValid and set busy
  // when(io.mem.req.fire) {
  //   memReqValid := false.B // request is sent, clear the valid state
  //   printf(cf"*ta*Memory request sent for register $reqTag at address $reqAddr.\n")
  // }

// // When the response comes back, clear busy
//   when(io.mem.resp.valid) {
//     memRespValid := true.B // response arrived
//     printf(cf"*ta*Memory response for tag ${io.mem.resp.bits.tag}.\n")
//   }

// Memory request interface
  io.mem.req.valid        := memReqValid                        // && !busy
  io.mem.req.bits.addr    := reqAddr
  io.mem.req.bits.tag     := reqTag
  // io.mem.req.bits.cmd     := Mux(state === STORE, M_XWR, M_XRD) // Write if in STORE state, otherwise Read
  io.mem.req.bits.cmd     := M_XRD
  io.mem.req.bits.size    := log2Ceil(4).U                     // 32 -> max 65536
  io.mem.req.bits.signed  := false.B
  io.mem.req.bits.data    := reqData
  io.mem.req.bits.phys    := false.B
  io.mem.req.bits.dprv    := cmd.bits.status.dprv
  io.mem.req.bits.dv      := cmd.bits.status.dv
  io.mem.req.bits.no_resp := false.B

// Ready/response logic
  cmd.ready := (state === IDLE)

  io.resp.valid   := state === CLEANUP
  io.resp.bits.rd := cmd.bits.inst.rd

  io.busy      := state =/= IDLE
  io.interrupt := false.B