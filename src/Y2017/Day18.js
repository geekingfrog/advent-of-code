'use strict';

// let a = 1;
// for (let i=31; i>0; i--) {
//   a*=2;
// }
// a-=1;
// console.log(a)
// console.log(Math.pow(2,31)-1)


// set i 31
// set a 1
// mul p 17
// jgz p p
// mul a 2
// add i -1
// jgz i -2
// add a -1
let a = Math.pow(2,31) - 1;
let b = 0;

let send = [];
let recover = [];

let p = 316;
for (let i=127; i>0; i--) {
  p *= 8505;
  p = p % a;
  p *= 129749;
  p += 12345;
  p = p % a;
  b = p % 10000;
  send.push(b);
}

let f = 0;
a = send[send.length-1];
console.log(a);

// while(a > 0) {
//   for (let i=126; i >= 0; i--) {
//     b = send[send.length-1];
//     p = b - a;
//
//     if (p === 0) {
//       send.push(a);
//       a = b;
//     } else {
//       send.push(b);
//       f = 1;
//     }
//   }
//   send.push(a);
// }
