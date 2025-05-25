module AS = Assem
val run_cfg : bool -> AS.program -> Cfg.cfgs
val run_liveness : bool -> Cfg.cfgs -> Liveness.program * bool
val run_regalloc : bool -> Cfg.cfgs -> Liveness.program -> AS.program
val run_all : bool -> bool -> AS.program -> AS.program
