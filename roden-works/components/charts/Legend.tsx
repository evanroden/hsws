interface LegendItem {
  label: string
  color: string
  /** Mirror the mark: rect for bars/areas, line for lines, dot for points */
  shape?: 'rect' | 'line' | 'dot'
}

export default function Legend({ items }: { items: LegendItem[] }) {
  return (
    <ul className="flex flex-wrap items-center gap-x-5 gap-y-2">
      {items.map((item) => (
        <li key={item.label} className="flex items-center gap-2 text-xs text-titanium">
          <Swatch color={item.color} shape={item.shape ?? 'rect'} />
          {item.label}
        </li>
      ))}
    </ul>
  )
}

export function Swatch({ color, shape }: { color: string; shape: 'rect' | 'line' | 'dot' }) {
  if (shape === 'line') return <span aria-hidden="true" className="inline-block w-4 h-0.5 rounded-full" style={{ background: color }} />
  if (shape === 'dot') return <span aria-hidden="true" className="inline-block w-2.5 h-2.5 rounded-full" style={{ background: color }} />
  return <span aria-hidden="true" className="inline-block w-3 h-3 rounded-[3px]" style={{ background: color }} />
}
