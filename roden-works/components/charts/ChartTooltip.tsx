import type { ReactNode } from 'react'

interface ChartTooltipProps {
  /** Anchor position in container pixels */
  x: number
  y: number
  containerWidth: number
  title?: ReactNode
  children: ReactNode
}

/**
 * Floating readout. Flips to the left of the anchor near the right edge so it
 * never overflows the chart. Values lead, labels follow.
 */
export default function ChartTooltip({ x, y, containerWidth, title, children }: ChartTooltipProps) {
  const flip = x > containerWidth * 0.62
  return (
    <div
      role="presentation"
      className="pointer-events-none absolute z-20 min-w-[168px] rounded-lg border border-white/10 bg-[#0D1417]/95 px-3 py-2.5 shadow-2xl shadow-black/40 backdrop-blur-sm"
      style={{
        left: x,
        top: y,
        transform: `translate(${flip ? 'calc(-100% - 14px)' : '14px'}, -50%)`,
      }}
    >
      {title && <div className="mb-1.5 text-[11px] font-medium uppercase tracking-wider text-muted">{title}</div>}
      <div className="space-y-1">{children}</div>
    </div>
  )
}

export function TooltipRow({ color, value, label }: { color?: string; value: ReactNode; label: ReactNode }) {
  return (
    <div className="flex items-center gap-2 text-xs">
      {color && <span aria-hidden="true" className="inline-block w-3 h-0.5 rounded-full shrink-0" style={{ background: color }} />}
      <span className="font-semibold text-white tabular-nums">{value}</span>
      <span className="text-muted">{label}</span>
    </div>
  )
}
