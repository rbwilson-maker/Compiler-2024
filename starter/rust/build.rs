// Build file to produce c0.rs
// Author: Miles Conn <mconn@andrew.cmu.edu>

use lalrpop::Configuration;

extern crate lalrpop;

fn main() {
  let mut config = Configuration::new();
  config.generate_in_source_tree();
  config.process_current_dir().unwrap();
}
