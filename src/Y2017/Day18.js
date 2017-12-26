'use strict';

const raw = [
  "set i 31",
  "set a 1",
  "mul p 17",
  "jgz p p",
  "mul a 2",
  "add i -1",
  "jgz i -2",
  "add a -1",
  "set i 127",
  "set p 316",
  "mul p 8505",
  "mod p a",
  "mul p 129749",
  "add p 12345",
  "mod p a",
  "set b p",
  "mod b 10000",
  "snd b",
  "add i -1",
  "jgz i -9",
  "jgz a 3",
  "rcv b",
  "jgz b -1",
  "set f 0",
  "set i 126",
  "rcv a",
  "rcv b",
  "set p a",
  "mul p -1",
  "add p b",
  "jgz p 4",
  "snd a",
  "set a b",
  "jgz 1 3",
  "snd b",
  "set f 1",
  "add i -1",
  "jgz i -11",
  "snd a",
  "jgz f -16",
  "jgz a -19",
];

const test = [
  "snd 1",
  "snd 2",
  "snd p",
  "rcv a",
  "rcv b",
  "rcv c",
  "rcv d",
];

function newProcess(ins, initialState) {
  // using js classes don't work with coroutines :/
  const instructions = ins;
  let playedBuffer = [];
  let state = {
    i:0,
    registers: initialState,
    playedBuffer: [],
    received: [],
    receivedPtr: 0
  };

  function getVal(x) {
    const y = parseInt(x);
    if (isNaN(y)) return state.registers[x] || 0;
    return y;
  }

  function binOp(f, a, b) {
    state.registers[a] = f(getVal(a), getVal(b));
    state.i++;
  }

  function* start() {
    while (state.i>=0 && state.i<instructions.length) {
      let instruction = instructions[state.i];
      yield* execInstruction(instruction);

    }
    return state;
  }

  function* execInstruction(instruction) {
    let [t, a, b] = instruction.split(' ');
    switch(t) {
      case 'set':
        binOp((_, b) => b, a, b)
        break;
      case 'add':
        binOp((a,b) => a+b, a, b);
        break;
      case 'mul':
        binOp((a,b) => a*b, a, b);
        break;
      case 'mod':
        binOp((a,b) => a%b, a, b);
        break;
      case 'jgz': {
        let x = getVal(a);
        if (x > 0) {
          state.i += getVal(b);
        } else {
          state.i++;
        }
        break;
      }
      case 'snd':
        state.playedBuffer.push(getVal(a));
        state.i++;
        break;
      case 'rcv':
        if (state.receivedPtr == state.received.length) {
          state.received = yield state.playedBuffer;
          state.receivedPtr = 0;
          state.playedBuffer = [];
        }

        state.registers[a] = state.received[state.receivedPtr];
        state.receivedPtr++;
        state.i++;
        break;
    }
  }

  return {start};
}

function scheduler(p0, p1) {

  let proc0 = {p: p0, sentCount: 0, started: false, done: false};
  let proc1 = {p: p1, sentCount: 0, started: false, done: false};

  let current = proc0;
  let other = proc1;

  while(!other.done) {
    let x = current.p.next(other.sent);
    current.started = true;
    current.done = x.done;

    if (!current.done) {
      let v = x.value || [];
      current.sent = v;
      current.sentCount += v.length;
    }

    // only relevant for the first call to the processes
    // since one process may not send anything when first yielding
    if (current.sentCount !== 0) [current, other] = [other, current];
  }

  return proc1.sentCount;
}

const p0 = newProcess(raw, {'p': 0});
const p1 = newProcess(raw, {'p': 1});

let result = scheduler(p0.start(), p1.start());
console.log(result);
