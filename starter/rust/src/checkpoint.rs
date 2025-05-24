// Author: Stephen McIntosh <semcinto@andrew.cmu.edu>

use serde::Deserialize;
use std::collections::HashMap;
use std::fs::File;
use std::io::prelude::*;
use std::io::{BufReader, BufWriter};
use std::path::{Path, PathBuf};

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Node {
  Register(String),
  Temporary(usize),
}

impl From<String> for Node {
  fn from(s: String) -> Self {
    let mut chars = s.chars().skip(1);
    if let Some(marker) = chars.next() {
      match marker {
        't' => Self::Temporary(chars.collect::<String>().parse().unwrap()),
        m => Self::Register(std::iter::once(m).chain(chars).collect()),
      }
    } else {
      panic!("Invalid node");
    }
  }
}

impl From<Node> for String {
  fn from(value: Node) -> Self {
    match value {
      Node::Register(s) => format!("%{:?}", s),
      Node::Temporary(n) => format!("%t{:?}", n),
    }
  }
}

#[derive(Debug, Deserialize)]
struct JsonAbstractLine {
  #[serde(rename = "Uses")]
  uses: Vec<String>,
  #[serde(rename = "Defines")]
  defines: Vec<String>,
  #[serde(rename = "Live_out")]
  live_out: Vec<String>,
  #[serde(rename = "Move")]
  is_move: bool,
}

#[derive(Debug)]
pub struct AbstractLine {
  uses: Vec<Node>,
  defines: Vec<Node>,
  live_out: Vec<Node>,
  is_move: bool,
}

impl From<JsonAbstractLine> for AbstractLine {
  fn from(line: JsonAbstractLine) -> Self {
    AbstractLine {
      uses: line.uses.into_iter().map(|x| x.into()).collect(),
      defines: line.defines.into_iter().map(|x| x.into()).collect(),
      live_out: line.live_out.into_iter().map(|x| x.into()).collect(),
      is_move: line.is_move,
    }
  }
}

type OutputLine = HashMap<String, String>;

#[allow(unreachable_code)]
#[allow(unused_variables)]
pub fn allocate(filename: &str) {
  let file = File::open(Path::new(filename)).unwrap();
  let mut reader = BufReader::new(file);

  // read and throw away the target line
  let mut throwaway_buf = String::new();
  reader.read_line(&mut throwaway_buf).unwrap();

  let raw: Vec<JsonAbstractLine> = serde_json::from_reader(reader).unwrap();
  let lines: Vec<AbstractLine> = raw.into_iter().map(|x| x.into()).collect();

  // do the register allocation here!

  let outputs: Vec<OutputLine> = panic!("not yet implemented");

  let mut out_file = PathBuf::from(filename);
  out_file.set_extension("out");
  let writer = BufWriter::new(File::create(out_file).unwrap());
  serde_json::to_writer(writer, &outputs).unwrap();
}
