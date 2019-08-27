let r = 44100, bf = 440, a = .002, ar = r * a, sounds = [], f, t, u, v, M = Math, bezier = x => x * x * (3 - 2 * x)
M.s = M.sin
M.c = (v, l = -1, m = 1) => M.max(M.min(v, m), l)
let cs = (k, s = {}) => ({
  ...s,
  s: t = s.s || k,
  v: M.c((u = k - t) < ar ? u / ar : M.log(1 - M.pow((u - ar) / r, 4)) + 1, 0, 1)
})

// let p = (x, f) => x * f / r
// let s1 = (x, f) => -.25 * M.s((v = x * f / r * M.PI) * 3) + .25 * M.s(v) + M.sqrt(3) / 2 * M.cos(v)
let ss = (i, f, p = 0, b = 0) => M.s(2 * M.PI * i / r * f + (p * M.PI) + b)

let fc = n => M.pow(2, ((n - 49) / 12)) * bf

let to = [1, .75, .25]

for (let k = 0; k < r * 10; k++) {
  if (k % (2 * r / 3) === 0) {
    let s = cs(k)
    s.f = fc(49 + 2 * k / r)
    // s.f = fc(49)
    sounds.push(s)
  }

  let byte = 0
  sounds.forEach((s, i) => {
    sounds[i] = cs(k, s)
    byte += ss(t = k - s.s, f = s.f, 0, to.reduce((r, m) => m + (1 - r) * ss(t, f, r), 0)) * s.v * (1 / sounds.length)
  })
  sounds = sounds.filter(s => s.s === k || s.v > 0)
  process.stdout.write(Buffer.from(Float32Array.of(M.c(byte, -.5, .5)).buffer))
}