/**
 * Horizontal bar outline with independent corner radii for the left and right
 * ends (0 = square). The command list is identical at every size, so
 * framer-motion can tween `d` from a zero-width bar to its full length.
 */
export function barPath(x: number, y: number, w: number, h: number, rLeft: number, rRight: number) {
  const width = Math.max(0, w)
  const l = Math.min(rLeft, width / 2, h / 2)
  const r = Math.min(rRight, width / 2, h / 2)
  const x1 = x + width
  return (
    `M${x + l},${y}H${x1 - r}A${r},${r} 0 0 1 ${x1},${y + r}V${y + h - r}` +
    `A${r},${r} 0 0 1 ${x1 - r},${y + h}H${x + l}A${l},${l} 0 0 1 ${x},${y + h - l}` +
    `V${y + l}A${l},${l} 0 0 1 ${x + l},${y}Z`
  )
}

let ctx: CanvasRenderingContext2D | null = null

/** Rendered width of a label in the page's sans face, so labels are only placed where they fit. */
export function measureText(text: string, size: number, weight = 400) {
  if (typeof document !== 'undefined') {
    ctx = ctx ?? document.createElement('canvas').getContext('2d')
    if (ctx) {
      ctx.font = `${weight} ${size}px ${getComputedStyle(document.body).fontFamily}`
      return ctx.measureText(text).width
    }
  }
  return text.length * size * 0.62
}

export const fmt = (n: number) => n.toLocaleString('en-US')
