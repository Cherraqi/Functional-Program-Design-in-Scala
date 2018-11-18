package week3

/** Digital circuit simulators */
object digitalCircuit {
  /* Digital circuits are composed of wires, which transport
   * signals (true / false) that are transformed by components.
   * base components are:
   * 	Inverter - output is inverse of inputs
   * 	AND Gate - output is conjunciton of inputs
   *  OR Gate - output is disjunction of inputs
   * The components have a delay (e.g., output doesn't change instantaneously)
   *
   */
	object sim extends Circuits with Parameters
	import sim._
	val in1, in2, sum, carry = new Wire   
                                                 
                                                
                                              
	
	halfAdder(in1, in2, sum, carry)
	probe("sum", sum)                       
	probe("carry", carry)                
	
	in1 setSignal true
	run()                                    
                                               
  in2 setSignal true
  run()                                  
                                               
                                               
  
  in1 setSignal false
  run()                                       
                                                
                                                
	
	
}
