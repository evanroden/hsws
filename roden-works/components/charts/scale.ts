export interface LinearScale {
  (value: number): number
  invert: (px: number) => number
  domain: [number, number]
  range: [number, number]
}

export function linearScale(domain: [number, number], range: [number, number]): LinearScale {
  const [d0, d1] = domain
  const [r0, r1] = range
  const k = d1 === d0 ? 0 : (r1 - r0) / (d1 - d0)
  const scale = ((v: number) => r0 + (v - d0) * k) as LinearScale
  scale.invert = (px: number) => (k === 0 ? d0 : d0 + (px - r0) / k)
  scale.domain = domain
  scale.range = range
  return scale
}

/** Round, human tick values (1/2/5 × 10^n steps) covering [min, max]. */
export function niceTicks(min: number, max: number, count = 5): number[] {
  if (max === min) return [min]
  const rough = (max - min) / count
  const mag = Math.pow(10, Math.floor(Math.log10(rough)))
  const norm = rough / mag
  const step = (norm >= 5 ? 10 : norm >= 2 ? 5 : norm >= 1 ? 2 : 1) * mag
  const start = Math.ceil(min / step) * step
  const ticks: number[] = []
  for (let v = start; v <= max + step * 1e-9; v += step) ticks.push(Number(v.toFixed(10)))
  return ticks
}

/** Upper bound rounded to the tick step so the top gridline is a clean value. */
export function niceMax(max: number, count = 5): number {
  const ticks = niceTicks(0, max, count)
  const step = ticks.length > 1 ? ticks[1] - ticks[0] : max
  return Math.ceil(max / step) * step
}

export const formatMillions = (v: number, digits = 1) => `$${v.toFixed(digits)}M`
