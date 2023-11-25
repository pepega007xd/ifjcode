use std::{
    collections::HashMap,
    env::args,
    fs::read_to_string,
    io::stdin,
    ops::{Add, Mul},
    process::exit,
};

fn print_help_exit() -> ! {
    eprintln!("Usage: ./ifjcode input_file");
    exit(1);
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum FrameType {
    Local,
    Global,
    Temp,
}

impl FrameType {
    fn parse(input: &str) -> Option<Self> {
        use FrameType::*;
        match input {
            "GF" => Some(Global),
            "LF" => Some(Local),
            "TF" => Some(Temp),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Var {
    frame: FrameType,
    name: String,
}

impl Var {
    fn parse(input: &str) -> Option<Self> {
        let (frame, name) = input.split_once('@')?;

        Some(Var {
            frame: FrameType::parse(frame)?,
            name: name.to_string(),
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Value {
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),
    Nil,
}

impl Add for Value {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs + rhs),
            (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs + rhs),
            _ => panic!("invalid add"),
        }
    }
}

impl Mul for Value {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs * rhs),
            (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs * rhs),
            _ => panic!("invalid add"),
        }
    }
}

#[derive(Debug, Clone)]
enum Symb {
    Var(Var),
    Value(Value),
}

impl Symb {
    fn parse(input: &str) -> Option<Self> {
        let (prefix, content) = input.split_once('@')?;

        use Symb::*;
        match prefix {
            a if matches!(FrameType::parse(a), Some(_)) => Some(Var(crate::Var::parse(input)?)),
            "int" => Some(Value(crate::Value::Int(content.parse::<i64>().ok()?))),
            "float" => Some(Value(crate::Value::Float(content.parse::<f64>().ok()?))),
            "string" => Some(Value(crate::Value::String(input.to_string()))),
            "nil" => Some(Value(crate::Value::Nil)),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
enum Ty {
    Int,
    Float,
    String,
    Bool,
}
impl Ty {
    fn parse(input: &str) -> Option<Self> {
        use Ty::*;
        match input {
            "int" => Some(Int),
            "float" => Some(Float),
            "string" => Some(String),
            "bool" => Some(Bool),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
enum Instruction {
    // basic and frame instructions
    Move(Var, Symb),
    Createframe,
    Pushframe,
    Popframe,
    Defvar(Var),
    Call(String),
    Return,
    // stack instructions
    Pushs(Symb),
    Pops(Var),
    Clears,
    // arithmetic instructions
    Add(Var, Symb, Symb),
    Mul(Var, Symb, Symb),
    Eq(Var, Symb, Symb),
    // I/O instructions
    Read(Var, Ty),
    Write(Symb),
    // type reflection
    Type(Var, Symb),
    // control flow instructions
    Label(String),
    Jump(String),
    Jumpifeq(String, Symb, Symb),
    Exit(Symb),
    // debug instructions
    Break,
    Dprint(Symb),
}
fn parse_instruction(line: &str) -> Instruction {
    let var = |input: Option<&str>| Var::parse(input.unwrap()).unwrap();
    let symb = |input: Option<&str>| Symb::parse(input.unwrap()).unwrap();
    let label = |input: Option<&str>| input.unwrap().to_string();

    let mut words = line.split_whitespace();
    let opcode = words.next().unwrap().to_lowercase();
    let arg1 = words.next();
    let arg2 = words.next();
    let arg3 = words.next();

    use Instruction::*;
    match opcode.as_str() {
        "move" => Move(var(arg1), symb(arg2)),
        "createframe" => Createframe,
        "pushframe" => Pushframe,
        "popframe" => Popframe,
        "defvar" => Defvar(var(arg1)),
        "call" => Call(label(arg1)),
        "return" => Return,
        "pushs" => Pushs(symb(arg1)),
        "pops" => Pops(var(arg1)),
        "clears" => Clears,
        "add" => Add(var(arg1), symb(arg2), symb(arg3)),
        "mul" => Mul(var(arg1), symb(arg2), symb(arg3)),
        "eq" => Eq(var(arg1), symb(arg2), symb(arg3)),
        "read" => Read(var(arg1), Ty::parse(arg2.unwrap()).unwrap()),
        "write" => Write(symb(arg1)),
        "type" => Type(var(arg1), symb(arg2)),
        "label" => Label(label(arg1)),
        "jump" => Jump(label(arg1)),
        "jumpifeq" => Jumpifeq(label(arg1), symb(arg2), symb(arg3)),
        "exit" => Exit(symb(arg1)),
        "break" => Break,
        "dprint" => Dprint(symb(arg1)),
        instr => panic!("unknown instruction: {:?}", instr),
    }
}

struct Program {
    instr: Vec<Instruction>,
    labels: HashMap<String, usize>,
}

fn parse_file(input: String) -> Program {
    let mut labels = HashMap::new();

    let instr = input
        .lines()
        .enumerate()
        .map(|(idx, line)| {
            // generic over years :5head:
            if line.is_empty() || line.starts_with(".IFJcode") {
                None
            } else {
                let instr = parse_instruction(line);
                if let Instruction::Label(ref label) = instr {
                    if let Some(_) = labels.insert(label.to_string(), idx) {
                        panic!("label redefinition")
                    }
                }
                Some(instr)
            }
        })
        .flatten()
        .collect::<Vec<_>>();

    Program { instr, labels }
}

type Frame = HashMap<Var, Option<Value>>;

#[derive(Default, Debug)]
struct State {
    global_frame: Frame,
    local_frames: Vec<Frame>,
    temp_frame: Option<Frame>,
    call_stack: Vec<usize>,
    value_stack: Vec<Value>,
}

impl State {
    fn get_frame(&self, var: &Var) -> &Frame {
        match var.frame {
            FrameType::Local => self.local_frames.last().expect("no local frame defined"),
            FrameType::Global => &self.global_frame,
            FrameType::Temp => self.temp_frame.as_ref().expect("no temp frame defined"),
        }
    }

    fn get_frame_mut(&mut self, var: &Var) -> &mut Frame {
        match var.frame {
            FrameType::Local => self
                .local_frames
                .last_mut()
                .expect("no local frame defined"),
            FrameType::Global => &mut self.global_frame,
            FrameType::Temp => self.temp_frame.as_mut().expect("no temp frame defined"),
        }
    }

    // returns the value stored in `symb`
    fn read_symb(&self, symb: Symb) -> Value {
        match symb {
            Symb::Var(var) => self
                .get_frame(&var)
                .get(&var)
                .expect("undefined variable")
                .as_ref()
                .expect("uninitialized variable")
                .clone(),
            Symb::Value(value) => value,
        }
    }

    fn define_var(&mut self, var: Var) {
        let frame = self.get_frame_mut(&var);
        if let Some(_) = frame.insert(var, None) {
            panic!("redefinition of variable")
        }
    }

    // writes `symb` into `var`
    fn write_var(&mut self, symb: Symb, var: Var) {
        let value = Some(self.read_symb(symb));
        let frame = self.get_frame_mut(&var);
        let var = frame.get_mut(&var);
        if let Some(var) = var {
            *var = value;
        } else {
            panic!("write to undefined var")
        }
    }
}

fn run_program(program: Program) {
    let mut ip = 0;
    let mut state = State::default();

    loop {
        let instr = program.instr.get(ip).unwrap_or_else(|| exit(0)).clone();
        ip += 1;

        match instr {
            Instruction::Move(var, symb) => state.write_var(symb, var),
            Instruction::Createframe => state.temp_frame = Some(HashMap::new()),
            Instruction::Pushframe => state.local_frames.push(
                state
                    .temp_frame
                    .take()
                    .expect("cannot pushframe, temp is uninitialized"),
            ),
            Instruction::Popframe => {
                state.temp_frame = Some(
                    state
                        .local_frames
                        .pop()
                        .expect("cannot popframe, there is no local frame"),
                );
            }
            Instruction::Defvar(var) => state.define_var(var),
            Instruction::Call(label) => {
                state.call_stack.push(ip);
                ip = *program.labels.get(&label).expect("undefined label");
            }
            Instruction::Return => {
                ip = state
                    .call_stack
                    .pop()
                    .expect("call stack is empty, cannot return")
            }
            Instruction::Pushs(symb) => state.value_stack.push(state.read_symb(symb)),
            Instruction::Pops(var) => {
                let value = state.value_stack.pop().expect("cannot pop, stack is empty");
                state.write_var(Symb::Value(value), var);
            }
            Instruction::Clears => state.value_stack.clear(),
            Instruction::Add(var, lhs, rhs) => state.write_var(
                Symb::Value(state.read_symb(lhs) + state.read_symb(rhs)),
                var,
            ),
            Instruction::Mul(var, lhs, rhs) => state.write_var(
                Symb::Value(state.read_symb(lhs) * state.read_symb(rhs)),
                var,
            ),
            Instruction::Eq(var, lhs, rhs) => state.write_var(
                Symb::Value(Value::Bool(state.read_symb(lhs) == state.read_symb(rhs))),
                var,
            ),
            Instruction::Read(var, ty) => {
                let mut buf = String::new();
                stdin().read_line(&mut buf).unwrap();
                let value = match ty {
                    Ty::Int => {
                        if let Ok(val) = buf.parse::<i64>() {
                            Value::Int(val)
                        } else {
                            Value::Nil
                        }
                    }
                    Ty::Float => {
                        if let Ok(val) = buf.parse::<i64>() {
                            Value::Int(val)
                        } else {
                            Value::Nil
                        }
                    }
                    Ty::String => Value::String(buf),
                    Ty::Bool => match buf.as_str() {
                        "true" => Value::Bool(true),
                        "false" => Value::Bool(false),
                        _ => Value::Nil,
                    },
                };
                state.write_var(Symb::Value(value), var);
            }
            Instruction::Write(symb) => {
                let value = state.read_symb(symb);
                match value {
                    Value::Int(v) => println!("{v}"),
                    Value::Float(v) => println!("{v} (uz je pul jedne v noci a fakt se mi nechce resit debilni ceckovsky formatovani floatu pro projekt kterej mi k nicemu neni)"),
                    Value::String(v) => println!("{v}"),
                    Value::Bool(v) => println!("{v}"),
                    Value::Nil => (),
                };
            }
            Instruction::Type(var, symb) => {
                let value = state.read_symb(symb);
                let ty = match value {
                    Value::Int(_) => "int",
                    Value::Float(_) => "float",
                    Value::String(_) => "string",
                    Value::Bool(_) => "bool",
                    Value::Nil => "",
                }
                .to_string();
                state.write_var(Symb::Value(Value::String(ty)), var);
            }
            Instruction::Label(_) => (), // :ez:
            Instruction::Jump(label) => {
                ip = *program.labels.get(&label).expect("undefined label");
            }
            Instruction::Jumpifeq(label, lhs, rhs) => {
                let lhs = state.read_symb(lhs);
                let rhs = state.read_symb(rhs);

                if lhs == rhs {
                    ip = *program.labels.get(&label).expect("undefined label");
                }
            }
            Instruction::Exit(symb) => {
                let value = state.read_symb(symb);
                if let Value::Int(val) = value {
                    if (0..=49).contains(&val) {
                        exit(val as i32)
                    } else {
                        panic!("wrong return value")
                    }
                }
            }
            Instruction::Break => {
                eprintln!("IP = {ip}");
                eprintln!("{:#?}", state)
            }
            Instruction::Dprint(symb) => {
                let value = state.read_symb(symb);
                match value {
                    Value::Int(v) => eprintln!("{v:?}"),
                    Value::Float(v) => eprintln!("{v:?}"),
                    Value::String(v) => eprintln!("{v:?}"),
                    Value::Bool(v) => eprintln!("{v:?}"),
                    Value::Nil => eprintln!("(nil)"),
                };
            }
        }
    }
}

fn main() {
    let input_file = args().nth(1);
    if let Some(input_file) = input_file {
        if input_file == "--help" || input_file == "-h" {
            print_help_exit();
        }

        match read_to_string(input_file) {
            Ok(input) => {
                let program = parse_file(input);
                run_program(program);
            }
            Err(err) => {
                eprintln!("{err}");
                exit(1)
            }
        }
    }
}
