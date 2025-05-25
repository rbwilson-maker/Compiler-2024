(* Rachompicole L3 Compiler 
 * Elaborated Abstract Assembly
 * Authors: Rachel Wilson and Nicole Fang
 * 
 * The goal of this phase is to take the abstract assembly
 * and insert any instructions that the convert phase shouldn't do.
 * It will "elaborate" mod, div, and fn calls to do appropriate moves before and after.
 * Similarly for sar and sal it will move to rcx. 
 * This will allow for more accurate liveness analysis which will aid in
 * register allocation. 
 *
 * Provides some guarantees:
 * - The shift value will always be in RCX at time of shift 
 * - RCX and shift's dest always interfere
 *
 * - The dest of a div will always be RAX
 * - The dest of a mod will always be RDX 
 * - The lhs of div and mod will always be RAX 
 * - The rhs will interfere with RDX
 * - The rhs of mod will also interfere with RDX
 *
 * - The dest of a call will always be RAX
 * - The first six arguments of a function call are moved
 *   into their correct registers
 * - A function is parameterized only by it's spilled arguments 
 *
 * Div: rax <- rax / rhs
 * Mod: rdx <- rax % rhs
 * Shift: dest <- lhs >> rcx
 * Call: rax <- f(spills)
 *)


(** Takes an abstract assembly program and hardcodes registers which must be
    used by the assembly. This also helps with liveness analysis and spilling
    function arguments. *)
val elab_assem : Assem.program -> Assem.program
